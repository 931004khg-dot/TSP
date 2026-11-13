;;; ==========================================================================
;;; 파일명: TSP_5C_DCL_and_Strut.lsp
;;; 구  분: DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part C)
;;; 역  할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-create-cip-dcl, tsp-cip-update-hpile-dims, tsp-cip-update-wale-dims, tsp-cip-update-mode, tsp-run-cip-dialog 외 4개]
;;; 포함 함수: tsp-create-cip-dcl, tsp-cip-update-hpile-dims, tsp-cip-update-wale-dims, tsp-cip-update-mode, tsp-run-cip-dialog, section-choice-dialog-callback, tsp-apply-settings-to-all, tsp-apply-soil-to-selected, tsp-apply-support-to-selected
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-create-cip-dcl : C.I.P 전용 DCL 다이얼로그 파일 생성
;;;;==========================================================================
(defun tsp-create-cip-dcl ( / dcl-file fn )
  (setq dcl-file (strcat (getvar "TEMPPREFIX") "tsp_cip.dcl"))
  (setq fn (open dcl-file "w"))
  (write-line "tsp_cip_dlg : dialog {" fn)
  (write-line "  label = \"C.I.P 설정\";" fn)
  (write-line "  : column {" fn)
  ;; 2. H-Pile 규격 및 치수 박스
  (write-line "    : text { label = \"H-Pile 규격\"; }" fn)
  (write-line "    : popup_list { key = \"cip_hpile_spec\"; width = 30; fixed_width = true; }" fn)
  (write-line "    : row {" fn)
  (write-line "      : edit_box { key = \"cip_hpile_h\"; width = 5; is_enabled = false; }" fn)
  (write-line "      : text { label = \"x\"; }" fn)
  (write-line "      : edit_box { key = \"cip_hpile_b\"; width = 5; is_enabled = false; }" fn)
  (write-line "      : text { label = \"x\"; }" fn)
  (write-line "      : edit_box { key = \"cip_hpile_tw\"; width = 5; is_enabled = false; }" fn)
  (write-line "      : text { label = \"/\"; }" fn)
  (write-line "      : edit_box { key = \"cip_hpile_tf\"; width = 5; is_enabled = false; }" fn)
  (write-line "    }" fn)
  (write-line "    : spacer { height = 0.5; }" fn)
  ;; 3. 띠장 규격 및 치수 박스
  (write-line "    : text { label = \"띠장 규격\"; }" fn)
  (write-line "    : popup_list { key = \"cip_wale_spec\"; width = 30; fixed_width = true; }" fn)
  (write-line "    : row {" fn)
  (write-line "      : edit_box { key = \"cip_wale_h\"; width = 5; is_enabled = false; }" fn)
  (write-line "      : text { label = \"x\"; }" fn)
  (write-line "      : edit_box { key = \"cip_wale_b\"; width = 5; is_enabled = false; }" fn)
  (write-line "      : text { label = \"x\"; }" fn)
  (write-line "      : edit_box { key = \"cip_wale_tw\"; width = 5; is_enabled = false; }" fn)
  (write-line "      : text { label = \"/\"; }" fn)
  (write-line "      : edit_box { key = \"cip_wale_tf\"; width = 5; is_enabled = false; }" fn)
  (write-line "    }" fn)
  (write-line "    : spacer { height = 0.5; }" fn)
  ;; 4. C.I.P 전용 배열 설정
  (write-line "    : row {" fn)
  (write-line "      : text { label = \"C.I.P 직경 (D)\"; width = 10; }" fn)
  (write-line "      : edit_box { key = \"cip_dia\"; width = 5; }" fn)
  (write-line "    }" fn)
  (write-line "    : spacer { height = 0.5; }" fn)
  (write-line "    : row {" fn)
  (write-line "      : text { label = \"배열 모드\"; width = 10; }" fn)
  (write-line "      : popup_list { key = \"cip_layout_mode\"; width = 15; }" fn)
  (write-line "      : text { label = \"겹침량(mm)\"; }" fn)
  (write-line "      : edit_box { key = \"cip_overlap\"; width = 5; }" fn)
  (write-line "    }" fn)
  (write-line "    : spacer { height = 0.5; }" fn)
  (write-line "    : row {" fn)
  (write-line "      : text { label = \"H-Pile 삽입\"; width = 10; }" fn)
  (write-line "      : popup_list { key = \"cip_interval\"; width = 15; }" fn)
  (write-line "    }" fn)
  (write-line "    : spacer { height = 0.5; }" fn)
  (write-line "    : row {" fn)
  (write-line "      : text { label = \"앵커 C.T.C = \"; }" fn)
  (write-line "      : edit_box { key = \"cip_anchor_n\"; width = 5; }" fn)
  (write-line "      : text { label = \" × CIP C.T.C\"; }" fn)
  (write-line "    }" fn)
  (write-line "    : spacer { height = 0.5; }" fn)
  (write-line "    : row {" fn)
  (write-line "      : text { label = \"그라우팅 선택\"; width = 10; }" fn)
  (write-line "      : popup_list { key = \"cip_grouting\"; width = 15; }" fn)
  (write-line "    }" fn)
  (write-line "    : spacer { height = 0.5; }" fn)
  (write-line "    : row {" fn)
  (write-line "      : text { label = \"그라우팅 간격\"; width = 10; }" fn)
  (write-line "      : popup_list { key = \"cip_grouting_space\"; width = 15; }" fn)
  (write-line "      : edit_box { key = \"cip_grouting_space_custom\"; width = 7; is_enabled = false; }" fn)
  (write-line "    }" fn)
  (write-line "    : spacer { height = 0.5; }" fn)
  (write-line "    : row {" fn)
  (write-line "      : text { label = \"그라우팅 기준선\"; width = 10; }" fn)
  (write-line "      : popup_list { key = \"cip_grouting_offset\"; width = 15; }" fn)
  (write-line "      : edit_box { key = \"cip_grouting_offset_custom\"; width = 7; is_enabled = false; }" fn)
  (write-line "    }" fn)
  (write-line "  }" fn)
  ;; 5. 하단 버튼 영역
  (write-line "  : spacer { height = 1.0; }" fn)
  (write-line "  : row {" fn)
  (write-line "    alignment = centered;" fn)
  (write-line "    : button { key = \"accept\"; label = \"확인\"; is_default = true; width = 12; }" fn)
  (write-line "    : button { key = \"back\"; label = \"이전\"; width = 12; }" fn)
  (write-line "    : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; width = 12; }" fn)
  (write-line "  }" fn)
  (write-line "}" fn)
  (close fn)
  dcl-file
)

;;;;==========================================================================
;;;; defun tsp-cip-update-hpile-dims : H-Pile 규격에 맞춰 치수 박스 및 직경(D) 자동 업데이트
;;;;==========================================================================
(defun tsp-cip-update-hpile-dims (idx-str / idx hpile-spec hpile-vals h)
  (setq idx (atoi idx-str))
  (setq hpile-spec (nth idx *tsp-std-wall-list*))
  (if hpile-spec
    (progn
      (setq hpile-vals (parse-h-spec hpile-spec))
      (setq h (nth 0 hpile-vals))
      ;; 치수 박스 표시 (비활성화 상태이므로 값만 갱신)
      (set_tile "cip_hpile_h" (itoa (nth 0 hpile-vals)))
      (set_tile "cip_hpile_b" (itoa (nth 1 hpile-vals)))
      (set_tile "cip_hpile_tw" (itoa (nth 2 hpile-vals)))
      (set_tile "cip_hpile_tf" (itoa (nth 3 hpile-vals)))
      ;; 직경(D) 자동 연동 로직
      (cond
        ((<= h 250) (set_tile "cip_dia" "450"))
        ((<= h 300) (set_tile "cip_dia" "500"))
        ((<= h 350) (set_tile "cip_dia" "600"))
        ((<= h 400) (set_tile "cip_dia" "700"))
        (t (set_tile "cip_dia" "500"))
      )
    )
  )
)

;;;;==========================================================================
;;;; defun tsp-cip-update-wale-dims : 띠장 규격에 맞춰 치수 박스 자동 업데이트
;;;;==========================================================================
(defun tsp-cip-update-wale-dims (idx-str / idx wale-spec wale-vals)
  (setq idx (atoi idx-str))
  (setq wale-spec (nth idx *tsp-std-wall-list*))
  (if wale-spec
    (progn
      (setq wale-vals (parse-h-spec wale-spec))
      (set_tile "cip_wale_h" (itoa (nth 0 wale-vals)))
      (set_tile "cip_wale_b" (itoa (nth 1 wale-vals)))
      (set_tile "cip_wale_tw" (itoa (nth 2 wale-vals)))
      (set_tile "cip_wale_tf" (itoa (nth 3 wale-vals)))
    )
  )
)

;;;;==========================================================================
;;;; defun tsp-cip-update-mode : 배열 모드 변경 시 겹침량 입력창 활성/비활성 제어
;;;;==========================================================================
(defun tsp-cip-update-mode (val)
  (if (= val "0") ; 맞닿음 (Tangent)
    (progn
      (mode_tile "cip_overlap" 1)
      (set_tile "cip_overlap" "0")
    )
    (progn ; 겹침 (Secant)
      (mode_tile "cip_overlap" 0)
      (if (or (not *tsp-cip-overlap*) (= *tsp-cip-overlap* "") (= *tsp-cip-overlap* "0"))
        (set_tile "cip_overlap" "50")
        (set_tile "cip_overlap" *tsp-cip-overlap*)
      )
    )
  )
)

;;;;==========================================================================
;;;; defun tsp-run-cip-dialog : C.I.P 설정 대화상자 실행 및 콜백 제어
;;;;==========================================================================
(defun tsp-run-cip-dialog
  ( / dcl-id dcl-file mode-list interval-list grouting-list space-list offset-list dcl-status
      bak-wall-type bak-cip-hpile-idx bak-cip-wale-idx bak-cip-dia
      bak-cip-mode-idx bak-cip-overlap bak-cip-equal-divide
      bak-cip-interval-idx bak-cip-grouting-idx bak-cip-grouting-space-idx bak-cip-grouting-space-custom
      bak-cip-grouting-offset-idx bak-cip-grouting-offset-custom sync-idx
 )
  (setq dcl-file (tsp-create-cip-dcl))
  (setq dcl-id (load_dialog dcl-file))
  (if (not (new_dialog "tsp_cip_dlg" dcl-id)) (exit))

  (setq bak-wall-type        *tsp-wall-type*)
  (setq bak-cip-hpile-idx    *tsp-cip-hpile-idx*)
  (setq bak-cip-wale-idx     *tsp-cip-wale-idx*)
  (setq bak-cip-dia          *tsp-cip-dia*)
  (setq bak-cip-mode-idx     *tsp-cip-mode-idx*)
  (setq bak-cip-overlap      *tsp-cip-overlap*)
  (if (not *tsp-cip-equal-divide*) (setq *tsp-cip-equal-divide* "1"))
  (setq bak-cip-equal-divide *tsp-cip-equal-divide*)
  (setq bak-cip-interval-idx *tsp-cip-interval-idx*)
  (if (not *tsp-cip-anchor-n*) (setq *tsp-cip-anchor-n* "1"))
  (setq bak-cip-anchor-n *tsp-cip-anchor-n*)
  (setq bak-cip-grouting-idx *tsp-cip-grouting-idx*)

  ;; 그라우팅 간격 및 오프셋 백업
  (if (not *tsp-cip-grouting-space-idx*) (setq *tsp-cip-grouting-space-idx* "0"))
  (if (not *tsp-cip-grouting-space-custom*) (setq *tsp-cip-grouting-space-custom* ""))
  (if (not *tsp-cip-grouting-offset-idx*) (setq *tsp-cip-grouting-offset-idx* "0"))
  (if (not *tsp-cip-grouting-offset-custom*) (setq *tsp-cip-grouting-offset-custom* ""))

  (setq bak-cip-grouting-space-idx *tsp-cip-grouting-space-idx*)
  (setq bak-cip-grouting-space-custom *tsp-cip-grouting-space-custom*)
  (setq bak-cip-grouting-offset-idx *tsp-cip-grouting-offset-idx*)
  (setq bak-cip-grouting-offset-custom *tsp-cip-grouting-offset-custom*)

  (if (and *tsp-wale-spec* (setq sync-idx (vl-position *tsp-wale-spec* *tsp-std-wall-list*)))
    (setq *tsp-cip-wale-idx* (itoa sync-idx))
  )

  (start_list "cip_hpile_spec") (mapcar 'add_list *tsp-std-wall-list*) (end_list)
  (start_list "cip_wale_spec") (mapcar 'add_list *tsp-std-wall-list*) (end_list)
  (setq mode-list '("맞닿음 (Tangent)" "겹침 (Secant)"))
  (setq interval-list '("모든 공 삽입 (1/1)" "1공 건너 삽입 (1/2)" "2공 건너 삽입 (1/3)"))
  (setq grouting-list '("D200" "D600" "없음"))
  (setq space-list '("200" "400" "600" "직접입력" "자동(접점)"))
  (setq offset-list '("기본값 (D)" "직접입력"))

  (start_list "cip_layout_mode") (mapcar 'add_list mode-list) (end_list)
  (start_list "cip_interval") (mapcar 'add_list interval-list) (end_list)
  (start_list "cip_grouting") (mapcar 'add_list grouting-list) (end_list)
  (start_list "cip_grouting_space") (mapcar 'add_list space-list) (end_list)
  (start_list "cip_grouting_offset") (mapcar 'add_list offset-list) (end_list)

  (set_tile "cip_hpile_spec" *tsp-cip-hpile-idx*)
  (set_tile "cip_wale_spec" *tsp-cip-wale-idx*)
  (set_tile "cip_dia" *tsp-cip-dia*)
  (set_tile "cip_layout_mode" *tsp-cip-mode-idx*)
  (set_tile "cip_overlap" *tsp-cip-overlap*)
  (set_tile "cip_equal_divide" *tsp-cip-equal-divide*)
  (set_tile "cip_interval" *tsp-cip-interval-idx*)
  (set_tile "cip_anchor_n" *tsp-cip-anchor-n*)
  (set_tile "cip_grouting" *tsp-cip-grouting-idx*)
  (set_tile "cip_grouting_space" *tsp-cip-grouting-space-idx*)
  (set_tile "cip_grouting_space_custom" *tsp-cip-grouting-space-custom*)
  (set_tile "cip_grouting_offset" *tsp-cip-grouting-offset-idx*)
  (set_tile "cip_grouting_offset_custom" *tsp-cip-grouting-offset-custom*)

  (tsp-cip-update-hpile-dims *tsp-cip-hpile-idx*)
  (tsp-cip-update-wale-dims *tsp-cip-wale-idx*)
  (tsp-cip-update-mode *tsp-cip-mode-idx*)

  (set_tile "cip_dia" *tsp-cip-dia*)
  (set_tile "cip_overlap" *tsp-cip-overlap*)

  (if (= *tsp-cip-grouting-space-idx* "3") (mode_tile "cip_grouting_space_custom" 0) (mode_tile "cip_grouting_space_custom" 1))
  (if (= *tsp-cip-grouting-offset-idx* "1") (mode_tile "cip_grouting_offset_custom" 0) (mode_tile "cip_grouting_offset_custom" 1))
  ;; 자동(접점) 선택 시 기준선은 자동 산출되므로 비활성화
  (if (= *tsp-cip-grouting-space-idx* "4")
    (progn (mode_tile "cip_grouting_offset" 1) (mode_tile "cip_grouting_offset_custom" 1))
    (progn (mode_tile "cip_grouting_offset" 0)
           (if (= *tsp-cip-grouting-offset-idx* "1") (mode_tile "cip_grouting_offset_custom" 0) (mode_tile "cip_grouting_offset_custom" 1)))
  )

  (action_tile "cip_hpile_spec" "(tsp-cip-update-hpile-dims $value)")
  (action_tile "cip_wale_spec" "(tsp-cip-update-wale-dims $value)")
  (action_tile "cip_layout_mode" "(tsp-cip-update-mode $value)")
  (action_tile "cip_interval" "(set_tile \"cip_anchor_n\" (itoa (1+ (atoi $value))))")
  (action_tile "cip_grouting_space" "(progn (if (= $value \"3\") (mode_tile \"cip_grouting_space_custom\" 0) (mode_tile \"cip_grouting_space_custom\" 1)) (if (= $value \"4\") (progn (mode_tile \"cip_grouting_offset\" 1) (mode_tile \"cip_grouting_offset_custom\" 1)) (progn (mode_tile \"cip_grouting_offset\" 0) (if (= (get_tile \"cip_grouting_offset\") \"1\") (mode_tile \"cip_grouting_offset_custom\" 0) (mode_tile \"cip_grouting_offset_custom\" 1)))))")
  (action_tile "cip_grouting_offset" "(if (= $value \"1\") (mode_tile \"cip_grouting_offset_custom\" 0) (mode_tile \"cip_grouting_offset_custom\" 1))")

  (action_tile "back" "(progn
    (setq *tsp-wall-type* bak-wall-type)
    (setq *tsp-cip-hpile-idx* bak-cip-hpile-idx)
    (setq *tsp-cip-wale-idx* bak-cip-wale-idx)
    (setq *tsp-cip-dia* bak-cip-dia)
    (setq *tsp-cip-mode-idx* bak-cip-mode-idx)
    (setq *tsp-cip-overlap* bak-cip-overlap)
    (setq *tsp-cip-equal-divide* bak-cip-equal-divide)
    (setq *tsp-cip-interval-idx* bak-cip-interval-idx)
    (setq *tsp-cip-anchor-n* bak-cip-anchor-n)
    (setq *tsp-cip-grouting-idx* bak-cip-grouting-idx)
    (setq *tsp-cip-grouting-space-idx* bak-cip-grouting-space-idx)
    (setq *tsp-cip-grouting-space-custom* bak-cip-grouting-space-custom)
    (setq *tsp-cip-grouting-offset-idx* bak-cip-grouting-offset-idx)
    (setq *tsp-cip-grouting-offset-custom* bak-cip-grouting-offset-custom)
    (done_dialog 2)
  )")

  (action_tile "cancel" "(progn
    (setq *tsp-wall-type* bak-wall-type)
    (setq *tsp-cip-hpile-idx* bak-cip-hpile-idx)
    (setq *tsp-cip-wale-idx* bak-cip-wale-idx)
    (setq *tsp-cip-dia* bak-cip-dia)
    (setq *tsp-cip-mode-idx* bak-cip-mode-idx)
    (setq *tsp-cip-overlap* bak-cip-overlap)
    (setq *tsp-cip-equal-divide* bak-cip-equal-divide)
    (setq *tsp-cip-interval-idx* bak-cip-interval-idx)
    (setq *tsp-cip-anchor-n* bak-cip-anchor-n)
    (setq *tsp-cip-grouting-idx* bak-cip-grouting-idx)
    (setq *tsp-cip-grouting-space-idx* bak-cip-grouting-space-idx)
    (setq *tsp-cip-grouting-space-custom* bak-cip-grouting-space-custom)
    (setq *tsp-cip-grouting-offset-idx* bak-cip-grouting-offset-idx)
    (setq *tsp-cip-grouting-offset-custom* bak-cip-grouting-offset-custom)
    (done_dialog 0)
  )")

  (action_tile "accept" "(progn
    (setq *tsp-wall-type* \"CIP\")
    (setq *tsp-cip-hpile-idx* (get_tile \"cip_hpile_spec\"))
    (setq *tsp-cip-wale-idx* (get_tile \"cip_wale_spec\"))
    (setq *tsp-wale-spec* (nth (atoi *tsp-cip-wale-idx*) *tsp-std-wall-list*))
    (setq *tsp-cip-dia* (get_tile \"cip_dia\"))
    (setq *tsp-cip-mode-idx* (get_tile \"cip_layout_mode\"))
    (setq *tsp-cip-overlap* (get_tile \"cip_overlap\"))
    (setq *tsp-cip-equal-divide* (get_tile \"cip_equal_divide\"))
    (setq *tsp-cip-interval-idx* (get_tile \"cip_interval\"))
    (setq *tsp-cip-anchor-n* (get_tile \"cip_anchor_n\"))
    (setq *tsp-cip-grouting-idx* (get_tile \"cip_grouting\"))
    (setq *tsp-cip-grouting-space-idx* (get_tile \"cip_grouting_space\"))
    (setq *tsp-cip-grouting-space-custom* (get_tile \"cip_grouting_space_custom\"))
    (setq *tsp-cip-grouting-offset-idx* (get_tile \"cip_grouting_offset\"))
    (setq *tsp-cip-grouting-offset-custom* (get_tile \"cip_grouting_offset_custom\"))
    (done_dialog 1)
  )")

  (setq dcl-status (start_dialog))
  (unload_dialog dcl-id)
  (vl-file-delete dcl-file)

  dcl-status
)

;;;;==========================================================================
;;;; defun section-choice-dialog-callback : 단면도 생성 시 세그먼트/모서리 선택 다이얼로그 콜백
;;;;==========================================================================
(defun section-choice-dialog-callback (dcl-path / dcl-id result draw-seg draw-cor)
  (setq dcl-id (load_dialog dcl-path))
  (if (new_dialog "tsp_section_choice" dcl-id)
    (progn
      (action_tile "accept" "(progn (setq draw-seg (= (get_tile \"chk_seg_sec\") \"1\")) (setq draw-cor (= (get_tile \"chk_cor_sec\") \"1\")) (if (or draw-seg draw-cor) (done_dialog 1) (alert \"최소 하나 이상의 단면도를 선택해야 합니다.\")))")
      (action_tile "cancel" "(done_dialog 0)")
      (setq result (start_dialog))
      (unload_dialog dcl-id)
      (if (= result 1)
        (list draw-seg draw-cor)
        nil
      )
    )
    nil
  )
)

;;;;==========================================================================
;;;; defun tsp-apply-settings-to-all : 선택한 세그먼트의 설정값을 모든 세그먼트에 일괄 적용
;;;;==========================================================================
(defun tsp-apply-settings-to-all (source-idx / source-data i new-list item new-item)
  (setq source-data (nth source-idx *segment-list*))
  (setq i 0 new-list '())
  (foreach item *segment-list*
    (if (= i source-idx)
      (setq new-list (append new-list (list item)))
      (progn
        (setq new-item (list
          (cons 'ID (cdr (assoc 'ID item)))
          (cons 'NAME (cdr (assoc 'NAME item)))
          (cons 'IS-DEFINED T)
          (cons 'WALL-TYPE (if (assoc 'WALL-TYPE source-data) (cdr (assoc 'WALL-TYPE source-data)) "HPILE"))
          (cons 'SECTION-DRAW (cdr (assoc 'SECTION-DRAW source-data)))
          (cons 'SOIL-DATA (cdr (assoc 'SOIL-DATA source-data)))
          (cons 'N-VALUES (cdr (assoc 'N-VALUES source-data)))
          (cons 'HOLE-NO (if (assoc 'HOLE-NO source-data) (cdr (assoc 'HOLE-NO source-data)) "BH-"))
          (cons 'WALL-SPEC (cdr (assoc 'WALL-SPEC source-data)))
          (cons 'WALL-CUSTOM (cdr (assoc 'WALL-CUSTOM source-data)))
          (cons 'WALE-SPEC (cdr (assoc 'WALE-SPEC source-data)))
          (cons 'WALE-CUSTOM (cdr (assoc 'WALE-CUSTOM source-data)))
          (cons 'CTC (cdr (assoc 'CTC source-data)))
          (cons 'TIMBER-THICKNESS (cdr (assoc 'TIMBER-THICKNESS source-data)))
          (cons 'MAX-DEPTH (cdr (assoc 'MAX-DEPTH source-data)))
          (cons 'EMBED-DEPTH (cdr (assoc 'EMBED-DEPTH source-data)))
          (cons 'CIP-MAX-DEPTH (if (assoc 'CIP-MAX-DEPTH source-data) (cdr (assoc 'CIP-MAX-DEPTH source-data)) "10.0"))
          (cons 'CIP-EMBED-DEPTH (if (assoc 'CIP-EMBED-DEPTH source-data) (cdr (assoc 'CIP-EMBED-DEPTH source-data)) "3.0"))
          (cons 'CIP-HPILE-IDX (if (assoc 'CIP-HPILE-IDX source-data) (cdr (assoc 'CIP-HPILE-IDX source-data)) "2"))
          (cons 'CIP-WALE-IDX (if (assoc 'CIP-WALE-IDX source-data) (cdr (assoc 'CIP-WALE-IDX source-data)) "2"))
          (cons 'CIP-DIA (if (assoc 'CIP-DIA source-data) (cdr (assoc 'CIP-DIA source-data)) "500"))
          (cons 'CIP-MODE-IDX (if (assoc 'CIP-MODE-IDX source-data) (cdr (assoc 'CIP-MODE-IDX source-data)) "0"))
          (cons 'CIP-OVERLAP (if (assoc 'CIP-OVERLAP source-data) (cdr (assoc 'CIP-OVERLAP source-data)) "0"))
          (cons 'CIP-EQUAL-DIVIDE (if (assoc 'CIP-EQUAL-DIVIDE source-data) (cdr (assoc 'CIP-EQUAL-DIVIDE source-data)) "1"))
          (cons 'CIP-INTERVAL-IDX (if (assoc 'CIP-INTERVAL-IDX source-data) (cdr (assoc 'CIP-INTERVAL-IDX source-data)) "2"))
          (cons 'CIP-ANCHOR-N
            (if (assoc 'CIP-ANCHOR-N source-data)
              (cdr (assoc 'CIP-ANCHOR-N source-data))
              (itoa (1+ (atoi (if (assoc 'CIP-INTERVAL-IDX source-data) (cdr (assoc 'CIP-INTERVAL-IDX source-data)) "2"))))
            )
          )
          (cons 'CIP-GROUTING-IDX (if (assoc 'CIP-GROUTING-IDX source-data) (cdr (assoc 'CIP-GROUTING-IDX source-data)) "0"))
          (cons 'CIP-GROUTING-SPACE-IDX (if (assoc 'CIP-GROUTING-SPACE-IDX source-data) (cdr (assoc 'CIP-GROUTING-SPACE-IDX source-data)) "0"))
          (cons 'CIP-GROUTING-SPACE-CUSTOM (if (assoc 'CIP-GROUTING-SPACE-CUSTOM source-data) (cdr (assoc 'CIP-GROUTING-SPACE-CUSTOM source-data)) ""))
          (cons 'CIP-GROUTING-OFFSET-IDX (if (assoc 'CIP-GROUTING-OFFSET-IDX source-data) (cdr (assoc 'CIP-GROUTING-OFFSET-IDX source-data)) "0"))
          (cons 'CIP-GROUTING-OFFSET-CUSTOM (if (assoc 'CIP-GROUTING-OFFSET-CUSTOM source-data) (cdr (assoc 'CIP-GROUTING-OFFSET-CUSTOM source-data)) ""))
          (cons 'WATER-CHK (cdr (assoc 'WATER-CHK source-data)))
          (cons 'WATER-DEPTH (cdr (assoc 'WATER-DEPTH source-data)))
          (cons 'SHOTCRETE-ENABLE (if (assoc 'SHOTCRETE-ENABLE source-data) (cdr (assoc 'SHOTCRETE-ENABLE source-data)) "1"))
          (cons 'SHOTCRETE-CHK (if (assoc 'SHOTCRETE-CHK source-data) (cdr (assoc 'SHOTCRETE-CHK source-data)) "0"))
          (cons 'SHOTCRETE-LAYER (if (assoc 'SHOTCRETE-LAYER source-data) (cdr (assoc 'SHOTCRETE-LAYER source-data)) ""))
          (cons 'SHOTCRETE-THICK (if (assoc 'SHOTCRETE-THICK source-data) (cdr (assoc 'SHOTCRETE-THICK source-data)) 0.0))
          (cons 'SUPPORT-LIST (cdr (assoc 'SUPPORT-LIST source-data)))
          (cons 'CORNER-BRACE
            (if (and (cdr (assoc 'CORNER-BRACE item)) (= (car (cdr (assoc 'CORNER-BRACE item))) "Y"))
              (cdr (assoc 'CORNER-BRACE source-data))
              (cdr (assoc 'CORNER-BRACE item))
            )
          )
          (cons 'UPGRADE-WALE (cdr (assoc 'UPGRADE-WALE item)))
          (cons 'V-START (cdr (assoc 'V-START item)))
          (cons 'V-END (cdr (assoc 'V-END item)))
          (cons 'ANGLE (cdr (assoc 'ANGLE item)))
          (cons 'LENGTH (cdr (assoc 'LENGTH item)))
        ))
        (setq new-list (append new-list (list new-item)))
      )
    )
    (setq i (1+ i))
  )
  (setq *segment-list* new-list)
  (setq *tsp-data-dirty* T)
)

;;;;==========================================================================
;;;; defun tsp-apply-soil-to-selected : 선택된 세그먼트들에 지반 정보 및 숏크리트 설정 복사
;;;;==========================================================================
(defun tsp-apply-soil-to-selected (source-idx target-indices / source-data soil-data n-vals hole-no w-chk w-depth s-enable s-chk s-layer s-thick max-dp embed-dp rock-dp i new-list item new-item key-pair)
  (setq source-data (nth source-idx *segment-list*))
  (setq soil-data (cdr (assoc 'SOIL-DATA source-data)))
  (setq n-vals (cdr (assoc 'N-VALUES source-data)))
  (setq hole-no (if (assoc 'HOLE-NO source-data) (cdr (assoc 'HOLE-NO source-data)) "BH-"))
  (setq w-chk (cdr (assoc 'WATER-CHK source-data)))
  (setq w-depth (cdr (assoc 'WATER-DEPTH source-data)))
  (setq s-enable (cdr (assoc 'SHOTCRETE-ENABLE source-data)))
  (setq s-chk (cdr (assoc 'SHOTCRETE-CHK source-data)))
  (setq s-layer (cdr (assoc 'SHOTCRETE-LAYER source-data)))
  (setq s-thick (cdr (assoc 'SHOTCRETE-THICK source-data)))
  (setq max-dp (cdr (assoc 'MAX-DEPTH source-data)))
  (setq embed-dp (cdr (assoc 'EMBED-DEPTH source-data)))
  (setq rock-dp (cdr (assoc 'ROCK-EMBED source-data)))

  (setq i 0 new-list '())
  (foreach item *segment-list*
    ;; 선택된 대상(target-indices)에 포함되어 있고, 원본 자신이 아닐 경우에만 덮어쓰기
    (if (and (member i target-indices) (/= i source-idx))
      (progn
        (setq new-item item)
        (foreach key-pair (list
            (cons 'SOIL-DATA soil-data) (cons 'N-VALUES n-vals) (cons 'HOLE-NO hole-no)
            (cons 'WATER-CHK w-chk) (cons 'WATER-DEPTH w-depth)
            (cons 'SHOTCRETE-ENABLE s-enable) (cons 'SHOTCRETE-CHK s-chk)
            (cons 'SHOTCRETE-LAYER s-layer) (cons 'SHOTCRETE-THICK s-thick)
            (cons 'MAX-DEPTH max-dp) (cons 'EMBED-DEPTH embed-dp) (cons 'ROCK-EMBED rock-dp))
          (if (assoc (car key-pair) new-item)
            (setq new-item (subst key-pair (assoc (car key-pair) new-item) new-item))
            (setq new-item (append new-item (list key-pair)))
          )
        )
        (setq new-list (append new-list (list new-item)))
      )
      ;; 대상이 아니면 원본 그대로 유지
      (setq new-list (append new-list (list item)))
    )
    (setq i (1+ i))
  )
  (setq *segment-list* new-list)
  (setq *tsp-data-dirty* T)
)

;;;;==========================================================================
;;;; defun tsp-apply-support-to-selected : 선택된 세그먼트들에 지보재 정보 복사
;;;;==========================================================================
(defun tsp-apply-support-to-selected (src-idx target-indices / src-data src-support i seg new-data)
  (setq src-data (nth src-idx *segment-list*))
  (setq src-support (cdr (assoc 'SUPPORT-LIST src-data)))
  (setq i 0)
  (setq new-data '())
  (foreach seg *segment-list*
    (if (and (member i target-indices) (/= i src-idx))
      (progn
        (setq seg (subst (cons 'SUPPORT-LIST src-support) (assoc 'SUPPORT-LIST seg) seg))
        (setq new-data (append new-data (list seg)))
      )
      (setq new-data (append new-data (list seg)))
    )
    (setq i (1+ i))
  )
  (setq *segment-list* new-data)
  (setq *tsp-data-dirty* T)
)
