;;; ==========================================================================
;;; 파일명: TSP_5I_DCL_and_Strut.lsp
;;; 구  분: DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part I)
;;; 역  할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [hpile-dialog-callback]
;;; 포함 함수: hpile-dialog-callback
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun hpile-dialog-callback : H-Pile 규격 설정 대화상자 콜백
;;;;==========================================================================
(defun hpile-dialog-callback (dcl-path / dcl-id result idx std-len val-h val-b val-tw val-tf grouting-list space-list offset-list)
  (setq dcl-id (load_dialog dcl-path))
  (if (new_dialog "tsp_hpile" dcl-id)
    (progn
      ;; 1. 규격 목록 정의 (전역 변수 사용 + User-defined 자동 추가)
      (start_list "hpile_spec")
      (mapcar 'add_list (append *tsp-std-wall-list* '("User-defined")))
      (end_list)
      (start_list "wale_spec")
      (mapcar 'add_list (append *tsp-std-wall-list* '("User-defined")))
      (end_list)
      ;; 그라우팅 목록 정의 (CIP와 동일한 구조 + 말뚝배면 기준선 옵션)
      (if (not *tsp-cip-grouting-idx*) (setq *tsp-cip-grouting-idx* "0"))
      (if (not *tsp-cip-grouting-space-idx*) (setq *tsp-cip-grouting-space-idx* "0"))
      (if (not *tsp-cip-grouting-space-custom*) (setq *tsp-cip-grouting-space-custom* ""))
      (if (not *tsp-cip-grouting-offset-idx*) (setq *tsp-cip-grouting-offset-idx* "0"))
      (if (not *tsp-cip-grouting-offset-custom*) (setq *tsp-cip-grouting-offset-custom* ""))
      (setq grouting-list '("D200" "D600" "없음"))
      ;; CIP 전용 "자동(접점)"(4) 값이 넘어온 경우 H-Pile 목록 범위로 보정
      (if (> (atoi *tsp-cip-grouting-space-idx*) 3) (setq *tsp-cip-grouting-space-idx* "0"))
      (setq space-list '("200" "400" "600" "직접입력"))
      (setq offset-list '("기본값 (H+D/2)" "직접입력" "말뚝배면 (H)"))
      (start_list "hpile_grouting") (mapcar 'add_list grouting-list) (end_list)
      (start_list "hpile_grouting_space") (mapcar 'add_list space-list) (end_list)
      (start_list "hpile_grouting_offset") (mapcar 'add_list offset-list) (end_list)
      ;; 표준 리스트 길이 (User-defined 판별용)
      (setq std-len (length *tsp-std-wall-list*))
      ;; 2. 초기값 설정 로직 (리스트에서 현재 값의 위치를 찾음)
      ;; [H-Pile 설정]
      (if (setq idx (vl-position *tsp-hpile-spec* *tsp-std-wall-list*))
        (set_tile "hpile_spec" (itoa idx))      ; 리스트에 있으면 해당 인덱스 선택
        (set_tile "hpile_spec" (itoa std-len))  ; 없으면 마지막(User-defined) 선택
      )
      ;; 사용자 정의 입력창 활성/비활성 상태 동기화
      (if (= (get_tile "hpile_spec") (itoa std-len))
        (progn
           (if *tsp-hpile-custom*
             (progn
               (set_tile "hpile_h" (itoa (nth 0 *tsp-hpile-custom*)))
               (set_tile "hpile_b" (itoa (nth 1 *tsp-hpile-custom*)))
               (set_tile "hpile_tw" (itoa (nth 2 *tsp-hpile-custom*)))
               (set_tile "hpile_tf" (itoa (nth 3 *tsp-hpile-custom*)))
             )
           )
           (mode_tile "hpile_h" 0) (mode_tile "hpile_b" 0) (mode_tile "hpile_tw" 0) (mode_tile "hpile_tf" 0)
        )
        (progn (mode_tile "hpile_h" 1) (mode_tile "hpile_b" 1) (mode_tile "hpile_tw" 1) (mode_tile "hpile_tf" 1))
      )
      ;; [Wale 설정]
      (if (setq idx (vl-position *tsp-wale-spec* *tsp-std-wall-list*))
        (set_tile "wale_spec" (itoa idx))
        (set_tile "wale_spec" (itoa std-len))
      )
      (if (= (get_tile "wale_spec") (itoa std-len))
        (progn
           (if *tsp-wale-custom*
             (progn
               (set_tile "wale_h" (itoa (nth 0 *tsp-wale-custom*)))
               (set_tile "wale_b" (itoa (nth 1 *tsp-wale-custom*)))
               (set_tile "wale_tw" (itoa (nth 2 *tsp-wale-custom*)))
               (set_tile "wale_tf" (itoa (nth 3 *tsp-wale-custom*)))
             )
           )
           (mode_tile "wale_h" 0) (mode_tile "wale_b" 0) (mode_tile "wale_tw" 0) (mode_tile "wale_tf" 0)
        )
        (progn (mode_tile "wale_h" 1) (mode_tile "wale_b" 1) (mode_tile "wale_tw" 1) (mode_tile "wale_tf" 1))
      )
      (set_tile "ctc" (rtos *tsp-ctc* 2 2))
      (set_tile "timber_thickness" (itoa *tsp-timber-thickness*))
      ;; [그라우팅 초기값 설정]
      (set_tile "hpile_grouting" *tsp-cip-grouting-idx*)
      (set_tile "hpile_grouting_space" *tsp-cip-grouting-space-idx*)
      (set_tile "hpile_grouting_space_custom" *tsp-cip-grouting-space-custom*)
      (set_tile "hpile_grouting_offset" *tsp-cip-grouting-offset-idx*)
      (set_tile "hpile_grouting_offset_custom" *tsp-cip-grouting-offset-custom*)
      (if (= *tsp-cip-grouting-idx* "2")
        (progn
          (mode_tile "hpile_grouting_space" 1)
          (mode_tile "hpile_grouting_space_custom" 1)
          (mode_tile "hpile_grouting_offset" 1)
          (mode_tile "hpile_grouting_offset_custom" 1)
        )
        (progn
          (mode_tile "hpile_grouting_space" 0)
          (mode_tile "hpile_grouting_offset" 0)
          (if (= *tsp-cip-grouting-space-idx* "3") (mode_tile "hpile_grouting_space_custom" 0) (mode_tile "hpile_grouting_space_custom" 1))
          (if (= *tsp-cip-grouting-offset-idx* "1") (mode_tile "hpile_grouting_offset_custom" 0) (mode_tile "hpile_grouting_offset_custom" 1))
        )
      )
      ;; 3. 액션 처리 (std-len 변수를 사용하여 동적으로 처리)
      (action_tile "hpile_spec" "(if (= (atoi $value) std-len) (progn (mode_tile \"hpile_h\" 0)(mode_tile \"hpile_b\" 0)(mode_tile \"hpile_tw\" 0)(mode_tile \"hpile_tf\" 0)) (progn (mode_tile \"hpile_h\" 1)(mode_tile \"hpile_b\" 1)(mode_tile \"hpile_tw\" 1)(mode_tile \"hpile_tf\" 1)))")
      (action_tile "wale_spec" "(if (= (atoi $value) std-len) (progn (mode_tile \"wale_h\" 0)(mode_tile \"wale_b\" 0)(mode_tile \"wale_tw\" 0)(mode_tile \"wale_tf\" 0)) (progn (mode_tile \"wale_h\" 1)(mode_tile \"wale_b\" 1)(mode_tile \"wale_tw\" 1)(mode_tile \"wale_tf\" 1)))")
      (action_tile "hpile_grouting" "(if (= $value \"2\") (progn (mode_tile \"hpile_grouting_space\" 1) (mode_tile \"hpile_grouting_space_custom\" 1) (mode_tile \"hpile_grouting_offset\" 1) (mode_tile \"hpile_grouting_offset_custom\" 1)) (progn (mode_tile \"hpile_grouting_space\" 0) (mode_tile \"hpile_grouting_offset\" 0) (if (= (get_tile \"hpile_grouting_space\") \"3\") (mode_tile \"hpile_grouting_space_custom\" 0) (mode_tile \"hpile_grouting_space_custom\" 1)) (if (= (get_tile \"hpile_grouting_offset\") \"1\") (mode_tile \"hpile_grouting_offset_custom\" 0) (mode_tile \"hpile_grouting_offset_custom\" 1))))")
      (action_tile "hpile_grouting_space" "(if (= $value \"3\") (mode_tile \"hpile_grouting_space_custom\" 0) (mode_tile \"hpile_grouting_space_custom\" 1))")
      (action_tile "hpile_grouting_offset" "(if (= $value \"1\") (mode_tile \"hpile_grouting_offset_custom\" 0) (mode_tile \"hpile_grouting_offset_custom\" 1))")
      (action_tile "accept"
        "(progn
           (setq *tsp-wall-type* \"HPILE\")
           ;; H-Pile 저장
           (setq idx (atoi (get_tile \"hpile_spec\")))
           (if (< idx std-len)
             (setq *tsp-hpile-spec* (nth idx *tsp-std-wall-list*)) ;; 리스트에서 선택
             (setq *tsp-hpile-spec* \"User-defined\" *tsp-hpile-custom* (list (atoi (get_tile \"hpile_h\")) (atoi (get_tile \"hpile_b\")) (atoi (get_tile \"hpile_tw\")) (atoi (get_tile \"hpile_tf\"))))
           )
           ;; Wale 저장
           (setq idx (atoi (get_tile \"wale_spec\")))
           (if (< idx std-len)
             (setq *tsp-wale-spec* (nth idx *tsp-std-wall-list*)) ;; 리스트에서 선택
             (setq *tsp-wale-spec* \"User-defined\" *tsp-wale-custom* (list (atoi (get_tile \"wale_h\")) (atoi (get_tile \"wale_b\")) (atoi (get_tile \"wale_tw\")) (atoi (get_tile \"wale_tf\"))))
           )
           ;; 기타 변수 및 차수그라우팅 설정 저장
           (setq *tsp-ctc* (atof (get_tile \"ctc\")))
           (setq *tsp-timber-thickness* (atoi (get_tile \"timber_thickness\")))
           (setq *tsp-cip-grouting-idx* (get_tile \"hpile_grouting\"))
           (setq *tsp-cip-grouting-space-idx* (get_tile \"hpile_grouting_space\"))
           (setq *tsp-cip-grouting-space-custom* (get_tile \"hpile_grouting_space_custom\"))
           (setq *tsp-cip-grouting-offset-idx* (get_tile \"hpile_grouting_offset\"))
           (setq *tsp-cip-grouting-offset-custom* (get_tile \"hpile_grouting_offset_custom\"))
           (done_dialog 1)
         )"
      )
      (action_tile "back" "(done_dialog 2)")
      (action_tile "cancel" "(done_dialog 0)")
      (setq result (start_dialog))
      (unload_dialog dcl-id)
      result
    )
  )
)
