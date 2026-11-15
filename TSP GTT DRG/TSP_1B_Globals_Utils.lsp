;;; ==========================================================================
;;; 파일명: TSP_1B_Globals_Utils.lsp
;;; 구  분: 전역 변수 및 공통/그룹 유틸리티 (Part B)
;;; 역  할: 해치 패턴(16종), 표준 H형강 규격, 전역 설정 변수, 레이어/해치 유틸리티, 프로젝트 저장/불러오기/자동매칭, 도면 그룹(ACAD_GROUP) 제어 [tsp-load-project-data, tsp-delete-project-data, tsp-pm-build-test-segs, tsp-pm-shape-match-p, tsp-pm-find-matches 외 7개]
;;; 포함 함수: tsp-load-project-data, tsp-delete-project-data, tsp-pm-build-test-segs, tsp-pm-shape-match-p, tsp-pm-find-matches, tsp-pm-tag-boundary, tsp-pm-choose-candidate, tsp-pm-auto-select-project, save-prompt-dialog-callback, tsp-get-entity-handle, tsp-get-intersection-point, tsp-clear-drawing-group
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-load-project-data : 데이터 불러오기 (형상일치 검증, 불일치 시 수동 데이터 매칭 연동 및 좌표 보호)
;;;;==========================================================================
(defun tsp-load-project-data (proj-name / data-pack dict-name saved-segments curr-len saved-len i curr-seg saved-seg shape-match curr-len-val saved-len-val curr-ang-val saved-ang-val merged-seg merged-list msg1 msg2 msg3 dcl-path mapping map-idx old-start-pt new-start-pt delta-x delta-y translated-deleted translated-added pt new-pt rec translated-support shifted-sup j item shifted-item updated-segment-list seg sup-list translated-seg-sup upg-data side new-upg-data translated-upg-pts)
  (setq dict-name "TSP_DATA_DICT")
  (setq data-pack (vlax-ldata-get dict-name proj-name))
  (if data-pack
    (progn
      (setq saved-segments (nth 0 data-pack))
      (setq curr-len (length *segment-list*))
      (setq saved-len (length saved-segments))
      (setq shape-match (= curr-len saved-len))
      (if shape-match
        (progn
          (setq i 0)
          (while (and (< i curr-len) shape-match)
            (setq curr-seg (nth i *segment-list*))
            (setq saved-seg (nth i saved-segments))
            (setq curr-len-val (cdr (assoc 'LENGTH curr-seg)))
            (setq saved-len-val (cdr (assoc 'LENGTH saved-seg)))
            (setq curr-ang-val (cdr (assoc 'ANGLE curr-seg)))
            (setq saved-ang-val (cdr (assoc 'ANGLE saved-seg)))
            (if (or (not (equal curr-len-val saved-len-val 0.1))
                    (not (equal curr-ang-val saved-ang-val 0.1)))
              (setq shape-match nil)
            )
            (setq i (1+ i))
          )
        )
      )
      (if (not shape-match)
        (progn
          ;; 형상 또는 세그먼트 개수가 다를 때: [수동 데이터 매칭] 버튼이 포함된 안내창 호출
          (if (/= curr-len saved-len)
            (setq msg1 "현재 경계선과 저장된 프로젝트의 세그먼트 개수가 달라 그대로 불러올 수 없습니다."
                  msg2 (strcat "(현재 선분 수: " (itoa curr-len) "개  /  저장된 선분 수: " (itoa saved-len) "개)")
                  msg3 "[수동 데이터 매칭]을 통해 각 세그먼트에 데이터를 개별 매칭하시겠습니까?")
            (setq msg1 "경계선의 형상(각 선분의 길이 또는 꺾임 각도)이 일치하지 않습니다."
                  msg2 "(동일 세그먼트 수이나 선분 길이/각도가 다름)"
                  msg3 "[수동 데이터 매칭]을 통해 각 세그먼트에 데이터를 개별 매칭하시겠습니까?")
          )
          (if (tsp-shape-mismatch-prompt msg1 msg2 msg3)
            (progn
              (setq dcl-path (strcat (getvar "TEMPPREFIX") "tsp.dcl"))
              (if (not (findfile dcl-path)) (setq dcl-path (create-tsp-dcl)))
              (setq mapping (tsp-csv-mapping-dialog dcl-path *segment-list* saved-segments))
              (if mapping
                (progn
                  (setq merged-list '() i 0)
                  (while (< i curr-len)
                    (setq curr-seg (nth i *segment-list*))
                    (setq map-idx (nth i mapping))
                    (if map-idx
                      (progn
                        (setq saved-seg (nth map-idx saved-segments))
                        (setq merged-list (append merged-list (list (tsp-merge-csv-seg-into-curr curr-seg saved-seg))))
                      )
                      (setq merged-list (append merged-list (list curr-seg)))
                    )
                    (setq i (1+ i))
                  )
                  (setq *segment-list* merged-list)
                  ;; 프로젝트 기본 전역 설정 동기화 (형상이 다르므로 이전 도면의 절대좌표 수동말뚝/까치발 기록은 초기화)
                  (setq *tsp-hpile-spec* (nth 1 data-pack))
                  (setq *tsp-hpile-custom* (nth 2 data-pack))
                  (setq *tsp-wale-spec* (nth 3 data-pack))
                  (setq *tsp-wale-custom* (nth 4 data-pack))
                  (setq *tsp-ctc* (nth 5 data-pack))
                  (setq *tsp-timber-thickness* (nth 6 data-pack))
                  (setq *tsp-max-excavation-depth* (nth 7 data-pack))
                  (setq *tsp-embedment-depth* (nth 8 data-pack))
                  (setq *tsp-soil-layers* (nth 9 data-pack))
                  (setq *tsp-water-chk* (nth 10 data-pack))
                  (setq *tsp-water-depth* (nth 11 data-pack))
                  (setq *tsp-support-list* (tsp-sanitize-support-list (nth 12 data-pack)))
                  (setq *tsp-scale-section* (nth 13 data-pack))
                  (setq *tsp-scale-plan* (nth 14 data-pack))
                  (setq *tsp-deleted-post-points* '())
                  (setq *tsp-added-post-points* '())
                  (vlax-ldata-put "TSP_DICT" "DELETED_POSTS" '())
                  (vlax-ldata-put "TSP_DICT" "ADDED_POSTS" '())
                  (vlax-ldata-put "TSP_DICT" "DELETED_CORBELS" '())
                  (setq *tsp-current-project-name* proj-name)
                  (princ (strcat "\n[시스템] 프로젝트 '" proj-name "' 의 데이터가 수동 매칭을 통해 적용되었습니다."))
                  T
                )
                (progn
                  (princ "\n[취소] 수동 데이터 매칭이 취소되었습니다.")
                  nil
                )
              )
            )
            nil
          )
        )
        (progn
          ;; 형상이 완벽히 일치하는 경우: 기존 1:1 병합 및 절대좌표 이동(Shift) 보정 수행
          (setq merged-list '())
          (setq i 0)
          (while (< i curr-len)
            (setq curr-seg (nth i *segment-list*))
            (setq saved-seg (nth i saved-segments))
            (setq merged-seg (list
              (cons 'ID (cdr (assoc 'ID curr-seg)))
              (cons 'NAME (cdr (assoc 'NAME saved-seg)))
              (cons 'IS-DEFINED (cdr (assoc 'IS-DEFINED saved-seg)))
              (cons 'SECTION-DRAW (cdr (assoc 'SECTION-DRAW saved-seg)))
              (cons 'SOIL-DATA (cdr (assoc 'SOIL-DATA saved-seg)))
              (cons 'N-VALUES (cdr (assoc 'N-VALUES saved-seg)))
              (cons 'HOLE-NO (if (assoc 'HOLE-NO saved-seg) (cdr (assoc 'HOLE-NO saved-seg)) "BH-"))
              (cons 'WALL-SPEC (cdr (assoc 'WALL-SPEC saved-seg)))
              (cons 'WALL-CUSTOM (cdr (assoc 'WALL-CUSTOM saved-seg)))
              (cons 'WALE-SPEC (cdr (assoc 'WALE-SPEC saved-seg)))
              (cons 'WALE-CUSTOM (cdr (assoc 'WALE-CUSTOM saved-seg)))
              (cons 'CTC (cdr (assoc 'CTC saved-seg)))
              (cons 'TIMBER-THICKNESS (cdr (assoc 'TIMBER-THICKNESS saved-seg)))
              (cons 'MAX-DEPTH (cdr (assoc 'MAX-DEPTH saved-seg)))
              (cons 'EMBED-DEPTH (cdr (assoc 'EMBED-DEPTH saved-seg)))
              (cons 'ROCK-EMBED (cdr (assoc 'ROCK-EMBED saved-seg)))
              (cons 'WATER-CHK (cdr (assoc 'WATER-CHK saved-seg)))
              (cons 'WATER-DEPTH (cdr (assoc 'WATER-DEPTH saved-seg)))
              (cons 'SHOTCRETE-ENABLE (cdr (assoc 'SHOTCRETE-ENABLE saved-seg)))
              (cons 'SHOTCRETE-CHK (cdr (assoc 'SHOTCRETE-CHK saved-seg)))
              (cons 'SHOTCRETE-LAYER (cdr (assoc 'SHOTCRETE-LAYER saved-seg)))
              (cons 'SHOTCRETE-THICK (cdr (assoc 'SHOTCRETE-THICK saved-seg)))
              (cons 'SUPPORT-LIST (tsp-sanitize-support-list (cdr (assoc 'SUPPORT-LIST saved-seg))))
              (cons 'CORNER-BRACE (cdr (assoc 'CORNER-BRACE saved-seg)))
              (cons 'UPGRADE-WALE (cdr (assoc 'UPGRADE-WALE saved-seg)))
              (cons 'WALL-TYPE (cdr (assoc 'WALL-TYPE saved-seg)))
              (cons 'CIP-MAX-DEPTH (cdr (assoc 'CIP-MAX-DEPTH saved-seg)))
              (cons 'CIP-EMBED-DEPTH (cdr (assoc 'CIP-EMBED-DEPTH saved-seg)))
              (cons 'CIP-HPILE-IDX (cdr (assoc 'CIP-HPILE-IDX saved-seg)))
              (cons 'CIP-WALE-IDX (cdr (assoc 'CIP-WALE-IDX saved-seg)))
              (cons 'CIP-DIA (cdr (assoc 'CIP-DIA saved-seg)))
              (cons 'CIP-MODE-IDX (cdr (assoc 'CIP-MODE-IDX saved-seg)))
              (cons 'CIP-OVERLAP (cdr (assoc 'CIP-OVERLAP saved-seg)))
              (cons 'CIP-EQUAL-DIVIDE (cdr (assoc 'CIP-EQUAL-DIVIDE saved-seg)))
              (cons 'CIP-INTERVAL-IDX (cdr (assoc 'CIP-INTERVAL-IDX saved-seg)))
              (cons 'CIP-ANCHOR-N (if (assoc 'CIP-ANCHOR-N saved-seg) (cdr (assoc 'CIP-ANCHOR-N saved-seg)) "1"))
              (cons 'CIP-GROUTING-IDX (cdr (assoc 'CIP-GROUTING-IDX saved-seg)))
              (cons 'CIP-GROUTING-SPACE-IDX (if (assoc 'CIP-GROUTING-SPACE-IDX saved-seg) (cdr (assoc 'CIP-GROUTING-SPACE-IDX saved-seg)) "0"))
              (cons 'CIP-GROUTING-SPACE-CUSTOM (if (assoc 'CIP-GROUTING-SPACE-CUSTOM saved-seg) (cdr (assoc 'CIP-GROUTING-SPACE-CUSTOM saved-seg)) ""))
              (cons 'CIP-GROUTING-OFFSET-IDX (if (assoc 'CIP-GROUTING-OFFSET-IDX saved-seg) (cdr (assoc 'CIP-GROUTING-OFFSET-IDX saved-seg)) "0"))
              (cons 'CIP-GROUTING-OFFSET-CUSTOM (if (assoc 'CIP-GROUTING-OFFSET-CUSTOM saved-seg) (cdr (assoc 'CIP-GROUTING-OFFSET-CUSTOM saved-seg)) ""))
              (cons 'SUB-ZONES (if (assoc 'SUB-ZONES saved-seg) (cdr (assoc 'SUB-ZONES saved-seg)) nil))
              ;; ---------------------------------------------------------
              (cons 'V-START (cdr (assoc 'V-START curr-seg)))
              (cons 'V-END (cdr (assoc 'V-END curr-seg)))
              (cons 'ANGLE (cdr (assoc 'ANGLE curr-seg)))
              (cons 'LENGTH (cdr (assoc 'LENGTH curr-seg)))
            ))
            (setq merged-list (append merged-list (list merged-seg)))
            (setq i (1+ i))
          )
          (setq *segment-list* merged-list)
          (setq *tsp-hpile-spec* (nth 1 data-pack))
          (setq *tsp-hpile-custom* (nth 2 data-pack))
          (setq *tsp-wale-spec* (nth 3 data-pack))
          (setq *tsp-wale-custom* (nth 4 data-pack))
          (setq *tsp-ctc* (nth 5 data-pack))
          (setq *tsp-timber-thickness* (nth 6 data-pack))
          (setq *tsp-max-excavation-depth* (nth 7 data-pack))
          (setq *tsp-embedment-depth* (nth 8 data-pack))
          (setq *tsp-soil-layers* (nth 9 data-pack))
          (setq *tsp-water-chk* (nth 10 data-pack))
          (setq *tsp-water-depth* (nth 11 data-pack))
          (setq *tsp-support-list* (tsp-sanitize-support-list (nth 12 data-pack)))
          (setq *tsp-scale-section* (nth 13 data-pack))
          (setq *tsp-scale-plan* (nth 14 data-pack))

          ;; 기존 프로젝트와의 호환성을 위한 방어 로직 (배열 크기 확인) 및 도면 딕셔너리 동기화
          (if (> (length data-pack) 15)
            (progn
              (setq *tsp-deleted-post-points* (nth 15 data-pack))
              (setq *tsp-added-post-points* (nth 16 data-pack))
              ;; [까치발 삭제] 삭제 목록 복원 (구버전 프로젝트는 빈 목록)
              (vlax-ldata-put "TSP_DICT" "DELETED_CORBELS" (if (> (length data-pack) 18) (nth 18 data-pack) '()))

              ;; 기존 경계선(old)과 현재 경계선(new) 간의 위치 이동량(Shift)을 계산하여 수동 편집 절대좌표들을 새 위치에 맞게 이동
              (setq old-start-pt (cdr (assoc 'V-START (nth 0 saved-segments))))
              (setq new-start-pt (cdr (assoc 'V-START (nth 0 *segment-list*))))
              (if (and old-start-pt new-start-pt)
                (progn
                  (setq delta-x (- (car new-start-pt) (car old-start-pt)))
                  (setq delta-y (- (cadr new-start-pt) (cadr old-start-pt)))

                  ;; 1. 삭제된 말뚝(DELETED_POSTS) 좌표 보정
                  (setq translated-deleted '())
                  (if *tsp-deleted-post-points*
                    (foreach pt *tsp-deleted-post-points*
                      (setq translated-deleted (append translated-deleted (list (list (+ (car pt) delta-x) (+ (cadr pt) delta-y) 0.0))))
                    )
                  )
                  (setq *tsp-deleted-post-points* translated-deleted)
                  (tsp-shift-deleted-corbels delta-x delta-y) ;; [까치발 삭제] 삭제 목록 좌표 보정

                  ;; 2. 추가된 말뚝(ADDED_POSTS) 좌표 보정
                  (setq translated-added '())
                  (if *tsp-added-post-points*
                    (foreach rec *tsp-added-post-points*
                      (setq pt (car rec))
                      (setq new-pt (list (+ (car pt) delta-x) (+ (cadr pt) delta-y) 0.0))
                      (setq translated-added (append translated-added (list (cons new-pt (cdr rec)))))
                    )
                  )
                  (setq *tsp-added-post-points* translated-added)

                  ;; 3. 버팀보 수동 지정 절대좌표(manual-pts) 보정 (*tsp-support-list* 갱신) 및 디버그
                  (princ (strcat "\n[디버그] 계산된 이동량 -> delta-x: " (rtos delta-x 2 4) ", delta-y: " (rtos delta-y 2 4)))
                  (setq translated-support '())
                  (if *tsp-support-list*
                    (foreach sup *tsp-support-list*
                      (setq shifted-sup '())
                      (setq j 0)
                      (foreach item sup
                        (if (= j 8)
                          (if item
                            (if (listp item)
                              (progn
                                (princ "\n[디버그] 원본 manual-pts(리스트): ") (princ item)
                                (setq shifted-item (mapcar (function (lambda (pt) (list (+ (car pt) delta-x) (+ (cadr pt) delta-y) (if (caddr pt) (caddr pt) 0.0)))) item))
                                (princ "\n[디버그] 보정된 manual-pts: ") (princ shifted-item)
                                (setq shifted-sup (append shifted-sup (list shifted-item)))
                              )
                              (progn
                                (princ "\n[디버그] 원본 manual-pts(문자열/단일값 바이패스): ") (princ item)
                                (setq shifted-sup (append shifted-sup (list item)))
                              )
                            )
                            (setq shifted-sup (append shifted-sup (list nil)))
                          )
                          (setq shifted-sup (append shifted-sup (list item)))
                        )
                        (setq j (1+ j))
                      )
                      (setq translated-support (append translated-support (list shifted-sup)))
                    )
                  )
                  (setq *tsp-support-list* translated-support)

                  ;; 4. *segment-list* 내의 SUPPORT-LIST 및 UPGRADE-WALE 보정된 좌표 갱신
                  (setq updated-segment-list '())
                  (if *segment-list*
                    (foreach seg *segment-list*
                      ;; 4-1. SUPPORT-LIST 보정
                      (setq sup-list (cdr (assoc 'SUPPORT-LIST seg)))
                      (if sup-list
                        (progn
                          (setq translated-seg-sup '())
                          (foreach sup sup-list
                            (setq shifted-sup '())
                            (setq j 0)
                            (foreach item sup
                              (if (= j 8)
                                (if item
                                  (if (listp item)
                                    (setq shifted-sup (append shifted-sup (list (mapcar (function (lambda (pt) (list (+ (car pt) delta-x) (+ (cadr pt) delta-y) (if (caddr pt) (caddr pt) 0.0)))) item))))
                                    (setq shifted-sup (append shifted-sup (list item)))
                                  )
                                  (setq shifted-sup (append shifted-sup (list nil)))
                                )
                                (setq shifted-sup (append shifted-sup (list item)))
                              )
                              (setq j (1+ j))
                            )
                            (setq translated-seg-sup (append translated-seg-sup (list shifted-sup)))
                          )
                          (setq seg (subst (cons 'SUPPORT-LIST translated-seg-sup) (assoc 'SUPPORT-LIST seg) seg))
                        )
                      )

                      ;; 4-2. UPGRADE-WALE 수동 지정 좌표 보정
                      (setq upg-data (cdr (assoc 'UPGRADE-WALE seg)))
                      (if (and upg-data (> (length upg-data) 2) (nth 2 upg-data))
                        (progn
                          (setq translated-upg-pts '())
                          (foreach pt-data (nth 2 upg-data)
                            (setq side (car pt-data) pt (cadr pt-data))
                            (setq new-pt (list (+ (car pt) delta-x) (+ (cadr pt) delta-y) (if (caddr pt) (caddr pt) 0.0)))
                            (setq translated-upg-pts (append translated-upg-pts (list (list side new-pt))))
                          )
                          (setq new-upg-data (list (nth 0 upg-data) (nth 1 upg-data) translated-upg-pts))
                          (setq seg (subst (cons 'UPGRADE-WALE new-upg-data) (assoc 'UPGRADE-WALE seg) seg))
                        )
                      )

                      (setq updated-segment-list (append updated-segment-list (list seg)))
                    )
                  )
                  (setq *segment-list* updated-segment-list)
                  ;; 앵커 평면각 기록(기준점 절대좌표)도 동일 이동량으로 보정
                  (setq *segment-list* (tsp-shift-anchor-angles-in-segs *segment-list* delta-x delta-y))
                  (setq *tsp-support-list* (tsp-shift-anchor-angles *tsp-support-list* delta-x delta-y))
                )
              )

              ;; 보정된 데이터를 현재 도면의 TSP_DICT에 물리적으로 기록하여 유실 방지
              (vlax-ldata-put "TSP_DICT" "DELETED_POSTS" *tsp-deleted-post-points*)
              (vlax-ldata-put "TSP_DICT" "ADDED_POSTS" *tsp-added-post-points*)
            )
            (progn
              (setq *tsp-deleted-post-points* '())
              (setq *tsp-added-post-points* '())
              (vlax-ldata-put "TSP_DICT" "DELETED_POSTS" '())
              (vlax-ldata-put "TSP_DICT" "ADDED_POSTS" '())
            )
          )

          (setq *tsp-current-project-name* proj-name)
          (princ (strcat "\n[시스템] 프로젝트 '" proj-name "' 의 설정값 업로드 완료."))
          T
        )
      )
    )
    (progn (alert "데이터를 불러올 수 없습니다.") nil)
  )
)

;;;;==========================================================================
;;;; defun tsp-delete-project-data : 데이터 딕셔너리에서 프로젝트 삭제
;;;;==========================================================================
(defun tsp-delete-project-data (proj-name / dict-name)
  (setq dict-name "TSP_DATA_DICT")
  (vlax-ldata-delete dict-name proj-name)
  (princ (strcat "\n[시스템] 프로젝트 '" proj-name "' 이(가) 삭제되었습니다."))
  T
)

;;;;==========================================================================
;;;; [프로젝트 자동매칭] 경계선 선택 시 프로젝트 자동 탐색 기능
;;;;  - 1순위: 경계선 객체에 각인된 프로젝트명(TSP_PROJ_NAME) + 형상 재검증
;;;;  - 2순위: TSP_DATA_DICT 전체 순회 형상 매칭(세그먼트 수, 각 선분 길이/각도)
;;;;  - 후보 다수: 기존 프로젝트 목록 팝업(tsp_load_list)으로 사용자 선택
;;;;  - 일치 없음: nil 반환 (호출부에서 새 프로젝트로 초기화)
;;;;  - 형상 허용오차는 기존 tsp-load-project-data 검증과 동일(길이 0.1, 각도 0.1 rad)
;;;;==========================================================================

;;;; defun tsp-pm-build-test-segs : 정점 리스트로 비교용 세그먼트(LENGTH/ANGLE) 생성 (init-segment-data와 동일 계산식)
(defun tsp-pm-build-test-segs (verts closed / n num i p1 p2 res)
  (setq n (length verts))
  (setq num (if closed n (1- n)))
  (setq i 0)
  (setq res '())
  (while (< i num)
    (setq p1 (nth i verts))
    (setq p2 (nth (if closed (rem (1+ i) n) (1+ i)) verts))
    (setq res (cons (list (cons 'LENGTH (distance p1 p2)) (cons 'ANGLE (angle p1 p2))) res))
    (setq i (1+ i))
  )
  (reverse res)
)

;;;; defun tsp-pm-shape-match-p : 두 세그먼트 리스트의 형상 일치 여부 (기존 불러오기 검증 기준과 동일)
(defun tsp-pm-shape-match-p (cur saved / ok i c s cl sl ca sa)
  (if (and (listp saved) (> (length cur) 0) (= (length cur) (length saved)))
    (progn
      (setq ok T)
      (setq i 0)
      (while (and ok (< i (length cur)))
        (setq c (nth i cur))
        (setq s (nth i saved))
        (setq cl (cdr (assoc 'LENGTH c)))
        (setq ca (cdr (assoc 'ANGLE c)))
        (setq sl (if (listp s) (cdr (assoc 'LENGTH s)) nil))
        (setq sa (if (listp s) (cdr (assoc 'ANGLE s)) nil))
        (if (not (and (numberp cl) (numberp sl) (numberp ca) (numberp sa)
                      (equal cl sl 0.1) (equal ca sa 0.1)))
          (setq ok nil)
        )
        (setq i (1+ i))
      )
      ok
    )
    nil
  )
)

;;;; defun tsp-pm-find-matches : 저장된 전체 프로젝트 중 형상이 일치하는 목록 반환 ((프로젝트명 . 매칭시작정점) ...)
;;;; 폐합선: C:TSP와 동일하게 방향을 표준화(CCW이면 반전)한 뒤, 시작점이 달라도 찾도록 순환 이동(cyclic shift) 비교
;;;; 개방선: 정방향/역방향 두 가지 순서를 비교. 반환 정점은 "현재 경계선 위의 정점"이므로 복사본 경계선에서도 안전
(defun tsp-pm-find-matches (verts closed / res name pack saved n k found orders ord test-verts)
  (setq res '())
  (setq n (length verts))
  (if closed
    (setq orders (list (if (= (get-polygon-orientation verts) 1) (reverse verts) verts)))
    (setq orders (list verts (reverse verts)))
  )
  (foreach name (tsp-get-project-list)
    (setq pack (vlax-ldata-get "TSP_DATA_DICT" name))
    (if (and pack (listp pack) (listp (car pack)))
      (progn
        (setq saved (car pack))
        (setq found nil)
        (if (and (> n 1) (= (length saved) (if closed n (1- n))))
          (foreach ord orders
            (setq k 0)
            (while (and (not found) (< k (if closed n 1)))
              (setq test-verts (if (= k 0) ord (tsp-shift-vertex-list ord k)))
              (if (tsp-pm-shape-match-p (tsp-pm-build-test-segs test-verts closed) saved)
                (progn
                  (setq found T)
                  (setq res (cons (cons name (car test-verts)) res))
                )
              )
              (setq k (1+ k))
            )
          )
        )
      )
    )
  )
  (reverse res)
)

;;;; defun tsp-pm-tag-boundary : 경계선 객체에 프로젝트명(TSP_PROJ_NAME) 각인 (유효한 객체일 때만)
(defun tsp-pm-tag-boundary (ent proj-name)
  (if (and ent (= (type ent) 'ENAME) (entget ent)
           proj-name (= (type proj-name) 'STR) (/= proj-name ""))
    (progn
      (vlax-ldata-put ent "TSP_PROJ_NAME" proj-name)
      T
    )
    nil
  )
)

;;;; defun tsp-pm-choose-candidate : 형상 일치 후보가 여러 개일 때 기존 프로젝트 목록 팝업(tsp_load_list)으로 선택
;;;; 선택 시 프로젝트명 반환, 취소 시 nil (삭제 버튼은 사고 방지를 위해 비활성화)
(defun tsp-pm-choose-candidate (dcl-path names / dcl-id idx res)
  (setq idx 0)
  (setq res 0)
  (setq dcl-id (load_dialog dcl-path))
  (if (and dcl-id (>= dcl-id 0) (new_dialog "tsp_load_list" dcl-id))
    (progn
      (start_list "proj_list")
      (mapcar 'add_list names)
      (end_list)
      (set_tile "proj_list" "0")
      (mode_tile "btn_del_proj" 1)
      (action_tile "proj_list" "(setq idx (atoi $value))")
      (action_tile "accept" "(done_dialog 1)")
      (action_tile "cancel" "(done_dialog 0)")
      (setq res (start_dialog))
      (unload_dialog dcl-id)
      (if (= res 1) (nth idx names) nil)
    )
    (progn
      (if (and dcl-id (>= dcl-id 0)) (unload_dialog dcl-id))
      (alert "프로젝트 선택창을 열 수 없습니다. 새 프로젝트로 시작합니다.")
      nil
    )
  )
)

;;;; defun tsp-pm-auto-select-project : 선택한 경계선에 맞는 프로젝트 탐색
;;;; 반환: (프로젝트명 . 매칭시작정점-현재경계선위) 또는 nil(일치 없음/선택 취소)
(defun tsp-pm-auto-select-project (boundary-ent vertices dcl-path / closed matches tag hit sel)
  (setq closed (is-closed-polyline boundary-ent))
  (setq matches (tsp-pm-find-matches vertices closed))
  (setq tag (vlax-ldata-get boundary-ent "TSP_PROJ_NAME"))
  (if (and tag (= (type tag) 'STR) (not (assoc tag matches)))
    (princ (strcat "\n[알림] 경계선에 각인된 프로젝트 '" tag "' 는 삭제되었거나 형상이 달라 사용하지 않습니다."))
  )
  (cond
    ((null matches)
     (princ "\n[시스템] 형상이 일치하는 저장 프로젝트가 없습니다.")
     nil
    )
    ((and tag (= (type tag) 'STR) (setq hit (assoc tag matches)))
     (princ (strcat "\n[시스템] 경계선에 각인된 프로젝트 '" tag "' 를 자동으로 불러옵니다."))
     hit
    )
    ((= (length matches) 1)
     (princ (strcat "\n[시스템] 형상이 일치하는 프로젝트 '" (car (car matches)) "' 를 자동으로 불러옵니다."))
     (car matches)
    )
    (t
     (princ (strcat "\n[시스템] 형상이 일치하는 프로젝트가 " (itoa (length matches)) "개 있습니다. 목록에서 불러올 프로젝트를 선택하세요. (취소 시 새 프로젝트)"))
     (setq sel (tsp-pm-choose-candidate dcl-path (mapcar 'car matches)))
     (if sel (assoc sel matches) nil)
    )
  )
)

;;;;==========================================================================
;;;; defun save-prompt-dialog-callback : 저장 버튼 클릭 시 팝업창 띄우기 (예:1, 아니오:2, 취소:0 반환)
;;;;==========================================================================
(defun save-prompt-dialog-callback (dcl-path / dcl-id result)
  (setq dcl-id (load_dialog dcl-path))
  (if (new_dialog "tsp_save_prompt" dcl-id)
    (progn
      (action_tile "btn_yes" "(done_dialog 1)")  ;; 다른 이름으로 저장
      (action_tile "btn_no" "(done_dialog 2)")   ;; 덮어쓰기
      (action_tile "cancel" "(done_dialog 0)")   ;; 취소
      (setq result (start_dialog))
      (unload_dialog dcl-id)
      result
    )
    0
  )
)

;;;;==========================================================================
;;;; defun tsp-save-confirm-dialog : 종료 시 저장 여부 3버튼 팝업
;;;;   반환값 : 1 = 예(새로저장) / 2 = 아니오(덮어쓰기) / 0 = 취소(종료 중단)
;;;;   [신규 추가] 기존 save-prompt-dialog-callback 은 변경하지 않음
;;;;==========================================================================
(defun tsp-save-confirm-dialog (/ dcl-file fh dcl-id res)
  (setq dcl-file (strcat (getvar "TEMPPREFIX") "tsp_save_confirm.dcl"))
  (setq fh (open dcl-file "w"))
  (if fh
    (progn
      (write-line "tsp_save_confirm : dialog { label = \"프로젝트 저장\";" fh)
      (write-line "  : text { label = \"변경된 데이터가 있습니다. 다른 이름으로 저장하시겠습니까?\"; }" fh)
      (write-line "  : text { label = \"예 : 새 이름으로 저장     아니오 : 현재 프로젝트에 덮어쓰기     취소 : 저장하지 않음\"; }" fh)
      (write-line "  : row { alignment = centered; fixed_width = true;" fh)
      (write-line "    : button { key = \"btn_yes\"; label = \"예(새로저장)\"; width = 16; is_default = true; }" fh)
      (write-line "    : button { key = \"btn_no\"; label = \"아니오(덮어쓰기)\"; width = 18; }" fh)
      (write-line "    : button { key = \"cancel\"; label = \"취소\"; width = 10; is_cancel = true; }" fh)
      (write-line "  }" fh)
      (write-line "}" fh)
      (close fh)
      (setq dcl-id (load_dialog dcl-file))
      (if (and dcl-id (>= dcl-id 0) (new_dialog "tsp_save_confirm" dcl-id))
        (progn
          (action_tile "btn_yes" "(done_dialog 1)")
          (action_tile "btn_no" "(done_dialog 2)")
          (action_tile "cancel" "(done_dialog 0)")
          (setq res (start_dialog))
          (unload_dialog dcl-id)
          (if res res 0)
        )
        (progn
          (if (and dcl-id (>= dcl-id 0)) (unload_dialog dcl-id))
          ;; [안전장치] DCL 생성/로드 실패 시 명령행 대체 입력으로 동일 동작 유지
          (initget "Yes No")
          (setq res (getkword "\n다른 이름으로 저장하시겠습니까? [예(새로저장)/아니오(덮어쓰기)] <아니오>: "))
          (if (= res "Yes") 1 2)
        )
      )
    )
    0
  )
)

;;;;==========================================================================
;;;; defun tsp-project-save-name-prompt : 프로젝트 저장 이름 입력/수정 팝업
;;;;   CSV 내보내기 창과 동일하게 프로젝트명을 즉시 수정할 수 있는 편집창 제공
;;;;   반환값 : 확정된 프로젝트명 문자열 / 취소 시 nil
;;;;==========================================================================
(defun tsp-project-save-name-prompt (default-name / dcl-file fh dcl-id res input-name)
  (if (or (null default-name) (= default-name ""))
    (setq default-name (tsp-generate-default-project-name))
  )
  (setq input-name default-name)
  (setq dcl-file (strcat (getvar "TEMPPREFIX") "tsp_proj_save_name.dcl"))
  (setq fh (open dcl-file "w"))
  (if fh
    (progn
      (write-line "tsp_proj_save_name : dialog { label = \"프로젝트 저장\";" fh)
      (write-line "  : text { label = \"저장할 프로젝트 이름을 입력하거나 수정하세요.\"; }" fh)
      (write-line "  : edit_box { key = \"proj_name\"; label = \"프로젝트명\"; edit_width = 40; }" fh)
      (write-line "  : row { alignment = centered; fixed_width = true;" fh)
      (write-line "    : button { key = \"accept\"; label = \"저장\"; width = 12; is_default = true; }" fh)
      (write-line "    : button { key = \"cancel\"; label = \"취소\"; width = 12; is_cancel = true; }" fh)
      (write-line "  }" fh)
      (write-line "}" fh)
      (close fh)
      (setq dcl-id (load_dialog dcl-file))
      (if (and dcl-id (>= dcl-id 0) (new_dialog "tsp_proj_save_name" dcl-id))
        (progn
          (set_tile "proj_name" default-name)
          (mode_tile "proj_name" 2)
          (action_tile "proj_name" "(setq input-name $value)")
          (action_tile "accept" "(setq input-name (get_tile \"proj_name\")) (done_dialog 1)")
          (action_tile "cancel" "(done_dialog 0)")
          (setq res (start_dialog))
          (unload_dialog dcl-id)
          (if (= res 1)
            (progn
              (setq input-name (vl-string-trim " \t" (if input-name input-name "")))
              (if (= input-name "") nil input-name)
            )
            nil
          )
        )
        (progn
          (if (and dcl-id (>= dcl-id 0)) (unload_dialog dcl-id))
          nil
        )
      )
    )
    nil
  )
)

;;; ==========================================================================
;;; [SECTION 2-1] 그룹 및 객체 관리 유틸리티 (Group & Entity Utils)
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-get-entity-handle : 엔티티 고유 핸들(Handle) ID를 추출
;;;;==========================================================================
(defun tsp-get-entity-handle (ent)
  (cdr (assoc 5 (entget ent)))
)

;;;;==========================================================================
;;;; defun tsp-get-intersection-point : 두 직선(무한)의 교차점을 계산
;;;;==========================================================================
(defun tsp-get-intersection-point (p1 p2 p3 p4)
  ;; nil 옵션은 선분이 아닌 무한 직선으로 간주하여 교차점을 찾음을 의미
  (inters p1 p2 p3 p4 nil)
)

;;;;==========================================================================
;;;; defun tsp-clear-drawing-group : 특정 경계선에 종속된 기존 작도 그룹을 찾아 안전하게 삭제 및 분해하는 함수
;;;;==========================================================================
(defun tsp-clear-drawing-group (boundary-ent / handle group-name dict group pair ent-to-del ent-list debug-grp-del)
  ;; 1. 경계선 핸들을 기반으로 그룹명 생성
  (setq handle (tsp-get-entity-handle boundary-ent))
  (setq group-name (strcat "TSP_GRP_" handle))
  ;; 2. ACAD_GROUP 딕셔너리 검색
  (setq dict (dictsearch (namedobjdict) "ACAD_GROUP"))
  ;; 3. 해당 그룹이 존재할 경우 삭제 절차 진행
  (if (and dict (dictsearch (cdr (assoc -1 dict)) group-name))
    (progn
      (setq group (dictsearch (cdr (assoc -1 dict)) group-name))
      (princ (strcat "\n[시스템] 기존 객체 그룹 '" group-name "' 물리적 삭제 준비 중..."))

      (setq ent-list '())
      ;; STEP 1: 그룹 내에서 삭제할 엔티티 리스트만 먼저 수집 (루프 중 삭제 절대 금지)
      (foreach pair group
        (if (= (car pair) 340)
          (setq ent-list (cons (cdr pair) ent-list))
        )
      )

      ;; STEP 2: 그룹 정의를 딕셔너리에서 먼저 제거하여 캐드 내부의 메모리 종속성 완전 파괴
      (dictremove (cdr (assoc -1 dict)) group-name)
      (princ "\n[시스템] 그룹 종속성 해제 완료. 개별 객체 삭제 시작...")

      ;; STEP 3: 종속성이 끊어진 상태에서 안전하게 물리적 삭제 (entdel) 진행
      (setq debug-grp-del 0)
      (foreach ent-to-del ent-list
        (if (entget ent-to-del)
          (progn
            ;; vl-catch-all-apply로 감싸 만에 하나 발생할 수 있는 에러 스킵
            (vl-catch-all-apply 'entdel (list ent-to-del))
            (setq debug-grp-del (1+ debug-grp-del))
          )
        )
      )
      (princ (strcat "\n[디버그: TSP] 메모리 충돌 없이 안전하게 물리적 삭제된 객체 수: " (itoa debug-grp-del)))
    )
    (princ (strcat "\n[디버그: TSP] 삭제할 그룹 '" group-name "'을 ACAD_GROUP에서 찾을 수 없습니다."))
  )
)
