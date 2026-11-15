;;; ==========================================================================
;;; 파일명: TSP_3D_Section.lsp
;;; 구  분: 단면도 및 지반 주상도 작도 (Part D)
;;; 역  할: 단면 띠장/유압잭/스크류잭 블록 정의, 지하수위 기호, 지반 주상도 및 N치 작도, H-Pile+토류판/CIP 수직 단면도, 단면 지보재/브래킷/숏크리트/그라우팅 작도 [tsp-filter-supports-for-section, draw-section-ground-line]
;;; 포함 함수: tsp-filter-supports-for-section, draw-section-ground-line
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-filter-supports-for-section : 단면도 작도 전 구간(SUB-ZONE) 세트 전환 및 지보재 필터링
;;;;==========================================================================
(defun tsp-filter-supports-for-section (dcl-path sup-list / result-list processed-depths cancelled item s-type s-depth same-depth-items dcl-id sel-idx disp-list dlg-res chosen has-anc has-str has-ranged zone-opts zone-list z-idx r-pts seg-data v-start v-ang seg-len iv iv-key unique-ivs sub-zones def-subzones sz z-id z-name s-m e-m w-type w-desc hole-str target-sz)
  (setq result-list '())
  (setq processed-depths '())
  (setq cancelled nil)

  ;; 1. 한 세그먼트 내 파이썬 분할 구간(SUB-ZONES) 또는 앵커/버팀보 공존/구간(Range) 지정 시 단면 기준 선택
  (setq has-anc nil has-str nil has-ranged nil unique-ivs '())
  (setq seg-data (if (and (boundp '*tsp-current-seg-idx*) *tsp-current-seg-idx* *segment-list*) (nth *tsp-current-seg-idx* *segment-list*) nil))
  (setq v-start (if seg-data (cdr (assoc 'V-START seg-data)) '(0.0 0.0 0.0)))
  (setq v-ang (if seg-data (cdr (assoc 'ANGLE seg-data)) 0.0))
  (setq seg-len (if seg-data (cdr (assoc 'LENGTH seg-data)) 20000.0))

  ;; 세그먼트 내 정의된 SUB-ZONES 추출
  (setq sub-zones (if seg-data (cdr (assoc 'SUB-ZONES seg-data)) nil))
  (setq def-subzones '())
  (foreach sz sub-zones
    (if (cdr (assoc 'IS-DEFINED sz))
      (setq def-subzones (append def-subzones (list sz)))
    )
  )

  ;; 파이썬에서 특정 세부 구간 인덱스를 직접 지정해 호출한 경우(*tsp-forced-subzone-id*) 팝업 없이 즉시 해당 구간 세트 로드
  (if (and (boundp '*tsp-forced-subzone-id*) *tsp-forced-subzone-id* def-subzones)
    (progn
      (setq target-sz nil)
      (foreach sz def-subzones
        (if (= (cdr (assoc 'ZONE-ID sz)) *tsp-forced-subzone-id*)
          (setq target-sz sz)
        )
      )
      (if target-sz
        (progn
          (tsp-load-subzone-to-globals target-sz)
          (setq sup-list *tsp-support-list*)
        )
      )
    )
    ;; 일반적인 경우: SUB-ZONES 또는 지보재 구간 선택창 표시
    (progn
      (foreach item sup-list
        (setq s-type (car item))
        (if (wcmatch s-type "*앵커*") (setq has-anc T))
        (if (wcmatch s-type "버팀보*") (setq has-str T))
        (if (tsp-get-support-item-range-pts item)
          (progn
            (setq has-ranged T)
            (setq iv (tsp-get-support-item-dist-interval item v-start v-ang seg-len))
            (setq iv-key (strcat (rtos (/ (car iv) 1000.0) 2 1) "m ~ " (rtos (/ (cdr iv) 1000.0) 2 1) "m"))
            (if (not (assoc iv-key unique-ivs))
              (setq unique-ivs (append unique-ivs (list (cons iv-key iv))))
            )
          )
        )
      )

      (if (or (> (length def-subzones) 0) (and has-anc has-str) has-ranged)
        (progn
          (setq zone-opts '())
          ;; [1순위] 파이썬에서 분할된 SUB-ZONES가 있으면 각 구간(벽체+지층+수위+지보재 세트)을 최상단에 배치
          (foreach sz def-subzones
            (setq z-id (cdr (assoc 'ZONE-ID sz)))
            (setq z-name (if (assoc 'ZONE-NAME sz) (cdr (assoc 'ZONE-NAME sz)) (strcat "제" (itoa z-id) "구간")))
            (setq s-m (cdr (assoc 'START-M sz)) e-m (cdr (assoc 'END-M sz)))
            (setq w-type (if (assoc 'WALL-TYPE sz) (cdr (assoc 'WALL-TYPE sz)) "HPILE"))
            (setq w-desc (if (= w-type "CIP")
                           (strcat "CIP D" (cdr (assoc 'CIP-DIA sz)) "+" (cdr (assoc 'WALL-SPEC sz)))
                           (strcat "H-Pile+토류판 (" (cdr (assoc 'WALL-SPEC sz)) ")")))
            (setq hole-str (if (assoc 'HOLE-NO sz) (cdr (assoc 'HOLE-NO sz)) "BH-1"))
            (setq zone-opts
              (append zone-opts
                (list (cons (strcat "★ " z-name " [" (rtos s-m 2 1) "m ~ " (rtos e-m 2 1) "m] 단면도  (벽체: " w-desc " | 시추공: " hole-str ")")
                            (cons 'SUBZONE sz)))
              )
            )
          )
          ;; [2순위] 전체 지보재 통합 및 앵커/버팀보 단독 옵션
          (setq zone-opts (append zone-opts (list (cons "전체 지보재 통합 단면도 (대표 벽체 + 모든 지보재 표시)" 'ALL))))
          (if (and has-anc has-str)
            (setq zone-opts (append zone-opts (list (cons "앵커(Earth Anchor) 적용 단면도만 작도" 'ANCHOR_ONLY)
                                                    (cons "버팀보(Strut) 적용 단면도만 작도" 'STRUT_ONLY))))
          )
          ;; SUB-ZONES가 없을 때만 기존의 수동 지정 구간(unique-ivs) 표시 (중복 표시 방지)
          (if (= (length def-subzones) 0)
            (foreach uv unique-ivs
              (setq zone-opts (append zone-opts (list (cons (strcat "지정 구간 단면도 [" (car uv) "] (해당 구간 포함 지보재)") (cdr uv)))))
            )
          )

          (if (> (length zone-opts) 1)
            (progn
              (setq dcl-id (load_dialog dcl-path))
              (if (new_dialog "tsp_subgrp_choice" dcl-id)
                (progn
                  (set_tile "subgrp_msg" "세그먼트 내 분할 구간/복합 지보재가 설정되어 있습니다. 작도할 단면을 선택하세요:")
                  (start_list "subgrp_list")
                  (mapcar 'add_list (mapcar 'car zone-opts))
                  (end_list)
                  (setq z-idx 0)
                  (set_tile "subgrp_list" "0")
                  (action_tile "subgrp_list" "(setq z-idx (atoi $value))")
                  (action_tile "accept" "(done_dialog 1)")
                  (action_tile "cancel" "(done_dialog 0)")
                  (setq dlg-res (start_dialog))
                  (unload_dialog dcl-id)
                  (if (= dlg-res 1)
                    (progn
                      (setq chosen (cdr (nth z-idx zone-opts)))
                      (cond
                        ((= chosen 'ALL) nil)
                        ((= chosen 'ANCHOR_ONLY)
                         (setq sup-list (vl-remove-if-not '(lambda (x) (wcmatch (car x) "*앵커*")) sup-list))
                        )
                        ((= chosen 'STRUT_ONLY)
                         (setq sup-list (vl-remove-if-not '(lambda (x) (wcmatch (car x) "버팀보*")) sup-list))
                        )
                        ;; ★ SUB-ZONE 선택 시: 해당 구간의 벽체 공법, 지층, 지하수위, 지보재 세트로 전역 변수 즉시 전환
                        ((and (consp chosen) (= (car chosen) 'SUBZONE))
                         (tsp-load-subzone-to-globals (cdr chosen))
                         (setq sup-list *tsp-support-list*)
                        )
                        ((consp chosen)
                         (setq sup-list
                           (vl-remove-if-not
                             '(lambda (x / x-iv mid-d)
                                (setq x-iv (tsp-get-support-item-dist-interval x v-start v-ang seg-len))
                                (setq mid-d (/ (+ (car chosen) (cdr chosen)) 2.0))
                                (and (<= (car x-iv) (+ mid-d 1.0)) (>= (cdr x-iv) (- mid-d 1.0)))
                              )
                             sup-list
                           )
                         )
                        )
                      )
                    )
                    (setq cancelled T)
                  )
                )
                (unload_dialog dcl-id)
              )
            )
          )
        )
      )
    )
  )

  ;; 2. 동일 굴착깊이에 Sub-group이 2개 이상일 때 사용자 선택 필터링
  (foreach item sup-list
    (if (not cancelled)
      (progn
        (setq s-type (car item))
        (setq s-depth (cadddr item))
        (if (wcmatch s-type "버팀보*")
          (if (not (vl-member-if '(lambda (d) (equal d s-depth 1e-3)) processed-depths))
            (progn
              (setq processed-depths (append processed-depths (list s-depth)))
              ;; 현재 깊이와 동일한 모든 버팀보 Sub-group 수집
              (setq same-depth-items '())
              (foreach cand sup-list
                (if (and (wcmatch (car cand) "버팀보*") (equal (cadddr cand) s-depth 1e-3))
                  (setq same-depth-items (append same-depth-items (list cand)))
                )
              )
              (if (> (length same-depth-items) 1)
                (progn
                  (setq dcl-id (load_dialog dcl-path))
                  (if (new_dialog "tsp_subgrp_choice" dcl-id)
                    (progn
                      (set_tile "subgrp_msg" (strcat "설치깊이 " (rtos s-depth 2 2) "m 구간에 Sub-group이 " (itoa (length same-depth-items)) "개 있습니다."))
                      (setq disp-list
                        (mapcar
                          '(lambda (x)
                             (strcat (nth 1 x) "  |  " (nth 6 x) "  |  " (nth 7 x)
                                     "  |  각도:" (rtos (+ 90.0 (atof (tsp-brace-opt-get (if (> (length x) 11) (nth 11 x) nil) 14 "0.0"))) 2 1) "도"))
                          same-depth-items
                        )
                      )
                      (start_list "subgrp_list")
                      (mapcar 'add_list disp-list)
                      (end_list)
                      (setq sel-idx 0)
                      (set_tile "subgrp_list" "0")
                      (action_tile "subgrp_list" "(setq sel-idx (atoi $value))")
                      (action_tile "accept" "(done_dialog 1)")
                      (action_tile "cancel" "(done_dialog 0)")
                      (setq dlg-res (start_dialog))
                      (unload_dialog dcl-id)
                      (if (= dlg-res 1)
                        (progn
                          (setq chosen (nth sel-idx same-depth-items))
                          (setq result-list (append result-list (list chosen)))
                        )
                        (setq cancelled T)
                      )
                    )
                    (progn
                      (unload_dialog dcl-id)
                      (setq result-list (append result-list (list (car same-depth-items))))
                    )
                  )
                )
                (setq result-list (append result-list (list (car same-depth-items))))
              )
            )
          )
          (setq result-list (append result-list (list item)))
        )
      )
    )
  )
  (if cancelled nil (cons 'OK result-list))
)

;;;;==========================================================================
;;;; defun draw-section-ground-line : 단면도 그리기 메인 진입점 (지반선 그리기 및 UCS 변환)
;;;;==========================================================================
(defun draw-section-ground-line (dcl-path selected-idx / loop-choice side start-pt end-pt layer-name ltype-name pt1 pt2 h-spec-list h b tw tf draw-success filtered-pack orig-sup-list)
  (setq *tsp-cancel-flag* nil) ; 취소 신호 초기화
  (setq loop-choice T)
  (setq draw-success nil)

  ;; 좌/우/단면정의 선택 무한 루프
  (while loop-choice
    (initget "L R S")
    (setq side (vl-catch-all-apply 'getkword (list "\n단면선택 [좌측반단면(L) / 우측반단면(R) / 단면정의(S)] <L>: ")))
    (if (vl-catch-all-error-p side)
      (progn (setq side nil loop-choice nil *tsp-cancel-flag* T)) ;; ESC 누르면 종료
      (progn
        (if (or (null side) (= side "")) (setq side "L"))
        (if (= side "S")
          (progn
            ;; 단면정의 창 호출 및 데이터 갱신
            (princ "\n[단면정의] 지반 정보를 수정합니다.")
            (if (= (soil-dialog-callback dcl-path) 1)
              (progn
                ;; 확인을 누르면 수정한 데이터를 현재 세그먼트에 완벽히 덮어쓰기
                (sync-globals-to-segment selected-idx)
                (setq *tsp-data-dirty* T)
                (princ "\n지반 정보가 성공적으로 저장되었습니다. 도면을 작도할 방향을 선택하세요.")
              )
              (princ "\n수정이 취소되었습니다.")
            )
            ;; 루프 계속 유지 (다시 L/R/S 질문으로 돌아감)
          )
          (progn
            ;; L 또는 R을 선택하면 루프를 종료하고 작도 단계로 넘어감
            (setq loop-choice nil)
          )
        )
      )
    )
  )

  ;; 작도 진행 (side가 설정되어 있고 S가 아닌 경우)
  (if (and side (/= side "S"))
    (progn
      ;; [Sub-group 선택] 한 세그먼트에 Sub-group이 2개 이상이면 어떤 Sub-group으로 단면을 그릴지 사용자에게 질문
      (setq filtered-pack (tsp-filter-supports-for-section dcl-path *tsp-support-list*))
      (if (null filtered-pack)
        (setq *tsp-cancel-flag* T)
        (progn
          (setq start-pt (vl-catch-all-apply 'getpoint (list "\n지반선 기준점 지정: ")))
          (if (vl-catch-all-error-p start-pt)
            (progn (setq start-pt nil) (setq *tsp-cancel-flag* T))
          )
          (if start-pt
            (progn
              (setq orig-sup-list *tsp-support-list*)
              (setq *tsp-support-list* (cdr filtered-pack))
              ;; UCS -> WCS 변환
              (setq pt1 (trans start-pt 1 0))
              (if (= side "L")
                (setq end-pt (polar start-pt pi 30000.0))
                (setq end-pt (polar start-pt 0.0 30000.0))
              )
              (setq layer-name "_지반선")
              (create-layer-if-not-exists layer-name "8")
              (setq ltype-name "DASHED")
              (if (not (tblsearch "LTYPE" ltype-name))
                (vl-catch-all-apply 'vla-load (list (vla-get-linetypes (vla-get-activedocument (vlax-get-acad-object))) ltype-name (if (= (getvar "MEASUREMENT") 0) "acad.lin" "acadiso.lin")))
              )
              (setq pt2 (trans end-pt 1 0))
              (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) (cons 6 ltype-name) (cons 48 (if (= (getvar "MEASUREMENT") 0) 1000.0 40.0)) '(90 . 2) '(70 . 0) (cons 10 (list (car pt1) (cadr pt1))) (cons 10 (list (car pt2) (cadr pt2)))))

              ;; 주상도 작도 호출
              (draw-soil-column pt2 side)

              ;;;; 벽체 타입에 따른 단면도 작도 함수 분기 처리
              (if (equal *tsp-wall-type* "CIP")
                (progn
                  (setq cip-h-spec (nth (atoi (if *tsp-cip-hpile-idx* *tsp-cip-hpile-idx* "2")) *tsp-std-wall-list*))
                  (setq *tsp-h-pile-size* (parse-h-spec cip-h-spec))
                  (setq *tsp-cip-dia* (atof (if (type *tsp-cip-dia*) *tsp-cip-dia* "500")))
                  ;; [통합 깊이 변수로 교체] CIP 단면 작도 호출
                  (draw-section-cip pt1 side *tsp-max-depth* *tsp-embed-depth* *tsp-support-list*)
                )
                (progn
                  (if (= *tsp-hpile-spec* "User-defined")
                    (setq h-spec-list *tsp-hpile-custom*)
                    (setq h-spec-list (parse-h-spec *tsp-hpile-spec*))
                  )
                  (setq h (nth 0 h-spec-list) b (nth 1 h-spec-list) tw (nth 2 h-spec-list) tf (nth 3 h-spec-list))
                  ;; H-Pile+토류판 단면 작도 호출
                  (draw-section-hpile pt1 side h b tw tf *tsp-max-depth* *tsp-embed-depth*)
                )
              )
              (setq *tsp-support-list* orig-sup-list)
              (if (and selected-idx (>= selected-idx 0) (< selected-idx (length *segment-list*)))
                (load-segment-to-globals selected-idx)
              )
              (setq draw-success T)
            )
            (if (and selected-idx (>= selected-idx 0) (< selected-idx (length *segment-list*)))
              (load-segment-to-globals selected-idx)
            )
          )
        )
      )
    )
  )
  draw-success
)
