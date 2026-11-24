;;; ==========================================================================
;;; 파일명: TSP_1C_Globals_Utils.lsp
;;; 구  분: 전역 변수 및 공통/그룹 유틸리티 (Part C)
;;; 역  할: 해치 패턴(16종), 표준 H형강 규격, 전역 설정 변수, 레이어/해치 유틸리티, 프로젝트 저장/불러오기/자동매칭, 도면 그룹(ACAD_GROUP) 제어 [tsp-group-last-entities, tsp-apply-custom-hatch, tsp-get-rock-start-depth]
;;; 포함 함수: tsp-group-last-entities, tsp-apply-custom-hatch, tsp-get-rock-start-depth
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-group-last-entities : 생성된 객체고유 그룹화
;;;;==========================================================================
(defun tsp-group-last-entities (start-ent boundary-ent / sset ent handle group-name dict acad-obj doc groups new-group valid-objs i obj-array)
  (setq handle (tsp-get-entity-handle boundary-ent))
  (setq group-name (strcat "TSP_GRP_" handle))

  (setq sset (ssadd))

  (if start-ent
    (progn
      ;; 안전하게 다음 객체 탐색 시도
      (setq ent (vl-catch-all-apply 'entnext (list start-ent)))
      (if (vl-catch-all-error-p ent)
        (setq ent nil)
      )
    )
    (progn
      (princ "\n[경고: TSP] 그룹화 시작점 (start-ent)이 nil입니다. 도면 처음부터 추적합니다.")
      (setq ent (entnext))
    )
  )

  (while ent
    (if (entget ent)
      (vl-catch-all-apply 'ssadd (list ent sset))
    )

    (setq ent (vl-catch-all-apply 'entnext (list ent)))
    (if (vl-catch-all-error-p ent)
      (setq ent nil)
    )
  )

  (if (> (sslength sset) 0)
    (progn
      ;; 1. ActiveX COM 객체 환경 초기화
      (setq acad-obj (vlax-get-acad-object))
      (setq doc (vla-get-activedocument acad-obj))
      (setq groups (vla-get-groups doc))

      ;; 2. 기존 그룹이 존재하면 삭제 (에러 무시)
      (vl-catch-all-apply 'vla-delete (list (vl-catch-all-apply 'vla-item (list groups group-name))))

      ;; 사전에서 한 번 더 찌꺼기 제거 (이중 안전장치)
      (setq dict (dictsearch (namedobjdict) "ACAD_GROUP"))
      (if (and dict (dictsearch (cdr (assoc -1 dict)) group-name))
        (dictremove (cdr (assoc -1 dict)) group-name)
      )

      ;; 3. 데이터베이스(ActiveX)에 그룹 생성
      (setq new-group (vl-catch-all-apply 'vla-add (list groups group-name)))

      (if (not (vl-catch-all-error-p new-group))
        (progn
          (vl-catch-all-apply 'vla-put-description (list new-group "TSP Auto-Gen"))

          ;; 4. 선택 세트(sset)에서 유효한 객체만 추출하여 배열로 변환 준비
          (setq valid-objs '())
          (setq i 0)
          (while (< i (sslength sset))
            (setq ent (ssname sset i))
            (if (entget ent)
              (setq valid-objs (cons (vlax-ename->vla-object ent) valid-objs))
            )
            (setq i (1+ i))
          )

          ;; 5. 안전 배열(SafeArray)로 묶어 그룹에 일괄 추가
          (if (> (length valid-objs) 0)
            (progn
              (setq valid-objs (reverse valid-objs))
              (setq obj-array (vlax-make-safearray vlax-vbobject (cons 0 (1- (length valid-objs)))))
              (setq i 0)
              (foreach obj valid-objs
                (vlax-safearray-put-element obj-array i obj)
                (setq i (1+ i))
              )
              (vl-catch-all-apply 'vla-appenditems (list new-group obj-array))
            )
          )
          (princ (strcat "\n[시스템] " (itoa (length valid-objs)) "개의 객체가 '" group-name "' 그룹으로 안전하게 생성되었습니다."))
        )
        (princ "\n[경고: TSP] 그룹 생성 중 ActiveX 에러가 발생했습니다.")
      )
    )
  )
)

;;;;==========================================================================
;;;; defun tsp-apply-custom-hatch : 커스텀 해치 패턴 PAT 생성 및 원점(Origin)/스케일 강제 지정
;;;;==========================================================================
(defun tsp-apply-custom-hatch (ent pat-name scale angle layer color origin-pt / support-dir pat-file f data old-echo old-hporiginmode old-hporigin old-hpannotative old-meas last-ent new-hatch)
  (setq old-echo (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  ;; 도면마다 스케일이 달라지는 현상 방지용 환경 변수 강제 통일
  (setq old-hpannotative (getvar "HPANNOTATIVE"))
  (setq old-meas (getvar "MEASUREMENT"))
  (if old-hpannotative (setvar "HPANNOTATIVE" 0)) ; 주석 축척 해제
  (setvar "MEASUREMENT" 1) ; 미터법 환경 강제 고정
  ;; 해치 원점 강제 지정 모드 설정
  (setq old-hporiginmode (getvar "HPORIGINMODE"))
  (setq old-hporigin (getvar "HPORIGIN"))
  (if origin-pt
    (progn
      (setvar "HPORIGINMODE" 0)
      (setvar "HPORIGIN" (list (car origin-pt) (cadr origin-pt)))
    )
  )
  (setq data (cdr (assoc pat-name *tsp-custom-pat-data*)))
  ;; Custom 패턴인 경우 .pat 파일 생성
  (if data
    (progn
      (setq support-dir (strcat (getvar "ROAMABLEROOTPREFIX") "Support"))
      (setq pat-file (strcat support-dir "\\" pat-name ".pat"))
      (setq f (open pat-file "w"))
      (foreach line data
        (write-line line f)
      )
      (write-line "" f)
      (close f)
    )
  )
  ;; 기본패턴(매립토 등)과 커스텀 구분 없이 모두 Command 방식으로 일괄 생성
  (setq last-ent (entlast))
  (command "_.-HATCH" "_P" pat-name scale angle "_S" ent "" "")
  ;; 방금 생성된 해치 객체의 레이어 및 색상 변경
  (setq new-hatch (entnext last-ent))
  (if new-hatch
    (command "_.CHPROP" new-hatch "" "_LA" layer "_C" color "")
  )
  ;; 시스템 변수 원상 복구 (기존 사용자 환경 보호)
  (if old-hporiginmode (setvar "HPORIGINMODE" old-hporiginmode))
  (if old-hporigin (setvar "HPORIGIN" old-hporigin))
  (if old-hpannotative (setvar "HPANNOTATIVE" old-hpannotative))
  (setvar "MEASUREMENT" old-meas)
  (setvar "CMDECHO" old-echo)
  (princ)
)

;;;;==========================================================================
;;;; defun tsp-get-rock-start-depth : 지층 데이터에서 숏크리트 적용 암반선의 심도를 탐색하여 반환 (단위: m)
;;;;==========================================================================
(defun tsp-get-rock-start-depth ( / rock-depth found-rock prev-depth l-name l-depth l-type)
  (setq rock-depth nil)
  (setq found-rock nil)
  (setq prev-depth 0.0)
  (if (and *tsp-shotcrete-enable* (= *tsp-shotcrete-enable* "1") *tsp-soil-layers*)
    (foreach layer *tsp-soil-layers*
      (if (not found-rock)
        (progn
          (setq l-name (car layer))
          ;; 문자열과 숫자 모두 대응
          (setq l-depth (if (= (type (cadr layer)) 'STR) (atof (cadr layer)) (cadr layer)))
          (setq l-type (if (> (length layer) 5) (nth 5 layer) nil))

          (if (and *tsp-shotcrete-chk* (= *tsp-shotcrete-chk* "1") *tsp-shotcrete-layer* (/= *tsp-shotcrete-layer* ""))
            (if (= l-name *tsp-shotcrete-layer*)
              (progn (setq rock-depth prev-depth) (setq found-rock T))
            )
            (if (or (and l-type (or (wcmatch l-type "*풍화암*") (wcmatch l-type "*연암*") (wcmatch l-type "*경암*")))
                    (and l-name (or (wcmatch l-name "*풍화암*") (wcmatch l-name "*연암*") (wcmatch l-name "*경암*"))))
              (progn (setq rock-depth prev-depth) (setq found-rock T))
            )
          )
          (setq prev-depth l-depth)
        )
      )
    )
  )
  rock-depth ; 찾은 심도(m) 반환, 못 찾으면 nil 반환
)
