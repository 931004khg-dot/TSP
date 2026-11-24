;;; ==========================================================================
;;; 파일명: TSP_5H_DCL_and_Strut.lsp
;;; 구  분: DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part H)
;;; 역  할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [soil-dialog-callback]
;;; 포함 함수: soil-dialog-callback
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun soil-dialog-callback : 지반 정보 입력 메인 대화상자 콜백
;;;;==========================================================================
(defun soil-dialog-callback (dcl-path / dcl-id result soil-data-list temp-layers temp-n-vals temp-hole soil-labels h-val phi-val r-idx sel-idx sel-item sel-name sel-depth sel-type-name sel-phi type-idx i user-name soil-depth-str soil-phi-str soil-depth soil-phi last-depth h-pattern h-scale h-angle new-layer new-list selected-type-info lg-mode pt leg-box n-hit n-pen n-depth-str n-depth n-str new-n-val new-n-list n-sel-idx n-sel-item slash-pos current-rock-list extracted-n-vals)
  (setq dcl-id (load_dialog dcl-path))
  (setq temp-layers (if *tsp-soil-layers* *tsp-soil-layers* '()))
  (setq temp-n-vals (if *tsp-n-values* *tsp-n-values* '()))
  (setq temp-hole (if *tsp-hole-no* *tsp-hole-no* "BH-"))
  (setq soil-data-list '(
    ("입도양호 자갈(GW)" "GW_" 100.0 0.0) ("입도불량 자갈(GP)" "GP_" 100.0 0.0)
    ("실트질 자갈(GM)" "GM_" 100.0 0.0) ("점토질 자갈(GC)" "GC_" 100.0 0.0)
    ("입도양호 모래(SW)" "SW_" 100.0 0.0) ("입도불량 모래(SP)" "SP_" 100.0 0.0)
    ("실트질 모래(SM)" "SM_" 100.0 0.0) ("점토질 모래(SC)" "SC_" 100.0 0.0)
    ("저소성 실트(ML)" "ML_" 100.0 0.0) ("고소성 실트(MH)" "MH_" 100.0 0.0)
    ("저소성 점토(CL)" "CL_" 100.0 0.0) ("고소성 점토(CH)" "CH_" 100.0 0.0)
    ("전석(BOULDER)" "GRAVEL" 30.0 0.0) ("풍화잔류토(RS)" "RS_" 2500.0 0.0)
    ("풍화암(WR)" "WR_" 100.0 0.0) ("연암(SR)" "SR_" 100.0 0.0)
    ("경암(HR)" "HR_" 100.0 0.0) ("기타" "LINE" 78.74 0.0)
  ))
  (setq soil-labels (mapcar 'car soil-data-list))
  (setq loop-dcl t)
  (setq result 0)
  (while loop-dcl
    (setq dcl-id (load_dialog dcl-path))
    (if (new_dialog "tsp_soil" dcl-id)
      (progn
        (start_list "soil_type") (mapcar 'add_list soil-labels) (end_list) (set_tile "soil_type" "0")

        ;; [팝업 리스트 업데이트]
        (defun update-rock-popup (/ rock-list l-name l-type)
          (setq rock-list '())
          (foreach layer temp-layers
            (setq l-name (car layer) l-type (nth 5 layer))
            (if (or (and l-type (or (wcmatch l-type "*풍화암*") (wcmatch l-type "*연암*") (wcmatch l-type "*경암*")))
                    (wcmatch l-name "*풍화암*") (wcmatch l-name "*연암*") (wcmatch l-name "*경암*"))
              (setq rock-list (append rock-list (list l-name)))
            )
          )
          (if (> (length rock-list) 0)
            (progn
              (start_list "soil_shotcrete_layer") (mapcar 'add_list rock-list) (end_list)
              (start_list "soil_cip_rock_layer") (mapcar 'add_list rock-list) (end_list)
            )
            (progn
              (start_list "soil_shotcrete_layer") (add_list "암반층 없음") (end_list)
              (start_list "soil_cip_rock_layer") (add_list "암반층 없음") (end_list)
            )
          )
          rock-list
        )

        ;; [숏크리트 UI 활성/비활성 제어]
        (defun toggle-shotcrete-ui (is-master-on has-rock)
          (if has-rock
            (progn
              (mode_tile "soil_shotcrete_enable" 0)
              (if is-master-on
                (progn
                  (mode_tile "soil_shotcrete_thick" 0)
                  (mode_tile "soil_shotcrete_chk" 0)
                  (if (= (get_tile "soil_shotcrete_chk") "1")
                    (mode_tile "soil_shotcrete_layer" 0)
                    (mode_tile "soil_shotcrete_layer" 1))
                )
                (progn
                  (mode_tile "soil_shotcrete_thick" 1)
                  (mode_tile "soil_shotcrete_chk" 1)
                  (mode_tile "soil_shotcrete_layer" 1)
                )
              )
            )
            (progn
              (set_tile "soil_shotcrete_enable" "0")
              (setq *tsp-shotcrete-enable* "0")
              (mode_tile "soil_shotcrete_enable" 1)
              (mode_tile "soil_shotcrete_thick" 1)
              (mode_tile "soil_shotcrete_chk" 1)
              (mode_tile "soil_shotcrete_layer" 1)
            )
          )
        )

        ;; [CIP 암반 정착 UI 활성/비활성 제어 - 신규 체크박스 연동]
        (defun toggle-cip-rock-ui (is-cip-mode has-rock)
          (if (and is-cip-mode has-rock)
            (progn
              (mode_tile "soil_rock_embed_chk" 0)
              ;; 암반 정착을 체크해야만 정착고 입력칸 활성화
              (if (= (get_tile "soil_rock_embed_chk") "1")
                (mode_tile "soil_rock_embed" 0)
                (mode_tile "soil_rock_embed" 1)
              )
              (mode_tile "soil_cip_rock_chk" 0)
              ;; 암반지정 체크해야만 팝업 리스트 활성화
              (if (= (get_tile "soil_cip_rock_chk") "1")
                (mode_tile "soil_cip_rock_layer" 0)
                (mode_tile "soil_cip_rock_layer" 1)
              )
            )
            (progn
              (mode_tile "soil_rock_embed_chk" 1)
              (mode_tile "soil_rock_embed" 1)
              (mode_tile "soil_cip_rock_chk" 1)
              (mode_tile "soil_cip_rock_layer" 1)
            )
          )
        )

        ;; [리스트 업데이트 및 연쇄 호출]
        (defun update-soil-list ()
          (start_list "soil_list")
          (foreach layer temp-layers
            (setq phi-val (if (nth 6 layer) (rtos (nth 6 layer) 2 1) "0.0"))
            (add_list (strcat (car layer) " - " (nth 5 layer) " - " (rtos (cadr layer) 2 2) "m - " phi-val "도")))
          (end_list)
          (setq current-rock-list (update-rock-popup))
          (toggle-shotcrete-ui (= (get_tile "soil_shotcrete_enable") "1") (> (length current-rock-list) 0))
          (toggle-cip-rock-ui (= *tsp-wall-type* "CIP") (> (length current-rock-list) 0))
        )

        (defun update-n-val-list ()
          (start_list "n_val_list")
          (foreach n-item temp-n-vals
            (add_list (strcat (cadr n-item) " - " (rtos (car n-item) 2 2) "m"))
          )
          (end_list)
        )

        ;; [N치 동작 관련 함수 생략 (내용은 그대로 유지됨)]
        (defun action-n-add (/ hit pen dep-str dep nstr nval)
          (setq hit (vl-string-trim " " (get_tile "n_hit")))
          (setq pen (vl-string-trim " " (get_tile "n_pen")))
          (setq dep-str (vl-string-trim " " (get_tile "n_depth")))
          (if (and dep-str (/= dep-str "") (numberp (read dep-str)))
            (progn (setq dep (atof dep-str)) (if (and (= hit "") (= pen "")) (alert "타격수/관입량 입력요망!") (progn (if (and (= hit "") (/= pen "")) (setq hit "50")) (if (and (/= hit "") (= pen "")) (setq pen "30")) (setq nstr (strcat hit "/" pen)) (setq nval (list dep nstr)) (setq temp-n-vals (append temp-n-vals (list nval))) (setq temp-n-vals (vl-sort temp-n-vals '(lambda (a b) (< (car a) (car b))))) (update-n-val-list) (set_tile "n_hit" "") (set_tile "n_pen" "") (set_tile "n_depth" "")))) (alert "깊이 오류!"))
        )
        (defun action-n-modify (/ n-sel-idx-str n-sel-idx hit pen dep-str dep nstr nval new-n-list i)
          (setq n-sel-idx-str (get_tile "n_val_list"))
          (if (and n-sel-idx-str (/= n-sel-idx-str ""))
            (progn (setq n-sel-idx (atoi n-sel-idx-str)) (setq hit (vl-string-trim " " (get_tile "n_hit"))) (setq pen (vl-string-trim " " (get_tile "n_pen"))) (setq dep-str (vl-string-trim " " (get_tile "n_depth"))) (if (and dep-str (/= dep-str "") (numberp (read dep-str))) (progn (setq dep (atof dep-str)) (if (and (= hit "") (= pen "")) (alert "타격수/관입량 입력요망!") (progn (if (and (= hit "") (/= pen "")) (setq hit "50")) (if (and (/= hit "") (= pen "")) (setq pen "30")) (setq nstr (strcat hit "/" pen)) (setq nval (list dep nstr)) (setq new-n-list '()) (setq i 0) (foreach item temp-n-vals (if (/= i n-sel-idx) (setq new-n-list (append new-n-list (list item)))) (setq i (1+ i))) (setq new-n-list (append new-n-list (list nval))) (setq temp-n-vals (vl-sort new-n-list '(lambda (a b) (< (car a) (car b))))) (update-n-val-list) (set_tile "n_hit" "") (set_tile "n_pen" "") (set_tile "n_depth" "")))) (alert "깊이 오류!"))) (alert "선택 오류!")
          )
        )
        (defun action-n-delete (/ n-sel-idx-str n-sel-idx new-n-list i)
          (setq n-sel-idx-str (get_tile "n_val_list"))
          (if (and n-sel-idx-str (/= n-sel-idx-str ""))
            (progn (setq n-sel-idx (atoi n-sel-idx-str)) (setq new-n-list '()) (setq i 0) (foreach item temp-n-vals (if (/= i n-sel-idx) (setq new-n-list (append new-n-list (list item)))) (setq i (1+ i))) (setq temp-n-vals new-n-list) (update-n-val-list) (set_tile "n_hit" "") (set_tile "n_pen" "") (set_tile "n_depth" "")) (alert "선택 오류!")
          )
        )
        (defun action-n-select (/ n-sel-idx n-sel-item n-str n-depth slash-pos)
          (setq n-sel-idx (atoi (get_tile "n_val_list")))
          (if (and temp-n-vals (>= n-sel-idx 0) (< n-sel-idx (length temp-n-vals)))
            (progn (setq n-sel-item (nth n-sel-idx temp-n-vals)) (setq n-str (cadr n-sel-item)) (setq n-depth (car n-sel-item)) (set_tile "n_depth" (rtos n-depth 2 2)) (setq slash-pos (vl-string-search "/" n-str)) (if slash-pos (progn (set_tile "n_hit" (substr n-str 1 slash-pos)) (set_tile "n_pen" (substr n-str (+ slash-pos 2)))) (progn (set_tile "n_hit" n-str) (set_tile "n_pen" ""))))
          )
        )

        ;; ---------------- UI 초기값 세팅 ----------------
        (set_tile "hole_no" temp-hole)        (update-n-val-list)
        (set_tile "soil_max_depth" (rtos *tsp-max-depth* 2 2))
        (set_tile "soil_embed_depth" (rtos *tsp-embed-depth* 2 2))

        ;; [신규] CIP 암반 정착 초기값 세팅
        (set_tile "soil_rock_embed_chk" (if *tsp-rock-embed-chk* *tsp-rock-embed-chk* "1"))
        (set_tile "soil_rock_embed" *tsp-rock-embed*)
        (set_tile "soil_cip_rock_chk" (if *tsp-cip-rock-chk* *tsp-cip-rock-chk* "0"))

        (set_tile "soil_shotcrete_enable" (if *tsp-shotcrete-enable* *tsp-shotcrete-enable* "1"))
        (if (or (null *tsp-shotcrete-thick*) (= *tsp-shotcrete-thick* 0.0))
          (progn
            (if (= *tsp-hpile-spec* "User-defined")
              (setq h-val (float (car *tsp-hpile-custom*)))
              (setq h-val (float (car (parse-h-spec *tsp-hpile-spec*))))
            )
            (setq *tsp-shotcrete-thick* (/ h-val 2.0))
          )
        )
        (set_tile "soil_shotcrete_thick" (rtos *tsp-shotcrete-thick* 2 0))
        (set_tile "soil_shotcrete_chk" (if *tsp-shotcrete-chk* *tsp-shotcrete-chk* "0"))

        (set_tile "soil_water_chk" (if *tsp-water-chk* *tsp-water-chk* "0"))
        (if (= (get_tile "soil_water_chk") "1") (progn (mode_tile "soil_water_depth" 0) (set_tile "soil_water_depth" (rtos *tsp-water-depth* 2 2))) (mode_tile "soil_water_depth" 1))

        (update-soil-list)

        (if (and *tsp-shotcrete-layer* (/= *tsp-shotcrete-layer* "") current-rock-list)
          (if (setq r-idx (vl-position *tsp-shotcrete-layer* current-rock-list))
            (set_tile "soil_shotcrete_layer" (itoa r-idx))
          )
        )
        (if (and *tsp-cip-rock-layer* (/= *tsp-cip-rock-layer* "") current-rock-list)
          (if (setq r-idx (vl-position *tsp-cip-rock-layer* current-rock-list))
            (set_tile "soil_cip_rock_layer" (itoa r-idx))
          )
        )

        ;; ---------------- 이벤트 (액션 타일) ----------------
        (action_tile "soil_water_chk" "(if (= $value \"1\") (mode_tile \"soil_water_depth\" 0) (mode_tile \"soil_water_depth\" 1))")
        (action_tile "soil_shotcrete_enable" "(progn (setq *tsp-shotcrete-enable* $value) (toggle-shotcrete-ui (= $value \"1\") (> (length current-rock-list) 0)))")
        (action_tile "soil_shotcrete_thick" "(setq *tsp-shotcrete-thick* (atof $value))")
        (action_tile "soil_shotcrete_chk" "(if (= $value \"1\") (mode_tile \"soil_shotcrete_layer\" 0) (mode_tile \"soil_shotcrete_layer\" 1))")

        ;; CIP 정착 체크박스 클릭 시 드롭다운 활성화 액션
        (action_tile "soil_cip_rock_chk" "(progn (setq *tsp-cip-rock-chk* $value) (if (= $value \"1\") (mode_tile \"soil_cip_rock_layer\" 0) (mode_tile \"soil_cip_rock_layer\" 1)))")

        ;; [신규] 암반 정착 사용 여부 토글 액션
        (action_tile "soil_rock_embed_chk" "(progn (setq *tsp-rock-embed-chk* $value) (if (= $value \"1\") (mode_tile \"soil_rock_embed\" 0) (mode_tile \"soil_rock_embed\" 1)))")

        (action_tile "soil_list" "(progn (setq sel-idx (atoi $value)) (if (and temp-layers (>= sel-idx 0) (< sel-idx (length temp-layers))) (progn (setq sel-item (nth sel-idx temp-layers)) (setq sel-name (car sel-item)) (setq sel-depth (cadr sel-item)) (setq sel-type-name (nth 5 sel-item)) (setq sel-phi (nth 6 sel-item)) (set_tile \"soil_user_name\" sel-name) (set_tile \"soil_depth\" (rtos sel-depth 2 2)) (if sel-phi (set_tile \"soil_phi\" (rtos sel-phi 2 1)) (set_tile \"soil_phi\" \"\")) (setq type-idx 0) (setq i 0) (foreach item soil-data-list (if (= (car item) sel-type-name) (setq type-idx i)) (setq i (1+ i))) (set_tile \"soil_type\" (itoa type-idx)))))")
        (action_tile "btn_add" "(progn (setq type-idx (atoi (get_tile \"soil_type\"))) (setq selected-type-info (nth type-idx soil-data-list)) (setq type-name (car selected-type-info)) (setq user-name (get_tile \"soil_user_name\")) (if (or (not user-name) (= user-name \"\")) (setq user-name (tsp-format-soil-name type-name))) (setq soil-depth-str (get_tile \"soil_depth\")) (setq soil-phi-str (get_tile \"soil_phi\")) (if (and soil-depth-str (/= soil-depth-str \"\") (numberp (read soil-depth-str))) (progn (setq soil-depth (atof soil-depth-str)) (setq soil-phi (if (numberp (read soil-phi-str)) (atof soil-phi-str) 0.0)) (setq last-depth (if temp-layers (cadr (last temp-layers)) 0.0)) (if (> soil-depth last-depth) (progn (setq h-pattern (nth 1 selected-type-info)) (setq h-scale (nth 2 selected-type-info)) (setq h-angle (nth 3 selected-type-info)) (setq new-layer (list user-name soil-depth h-pattern h-scale h-angle type-name soil-phi)) (setq temp-layers (append temp-layers (list new-layer))) (update-soil-list) (set_tile \"soil_user_name\" \"\") (set_tile \"soil_depth\" \"\") (set_tile \"soil_phi\" \"\") (mode_tile \"soil_user_name\" 2)) (alert \"깊이는 이전 지층보다 깊어야 합니다!\"))) (alert \"깊이를 올바르게 입력하세요!\")))")
        (action_tile "btn_modify" "(progn (setq sel-idx-str (get_tile \"soil_list\")) (if (and sel-idx-str (/= sel-idx-str \"\")) (progn (setq sel-idx (atoi sel-idx-str)) (setq type-idx (atoi (get_tile \"soil_type\"))) (setq selected-type-info (nth type-idx soil-data-list)) (setq type-name (car selected-type-info)) (setq user-name (get_tile \"soil_user_name\")) (if (or (not user-name) (= user-name \"\")) (setq user-name (tsp-format-soil-name type-name))) (setq soil-depth-str (get_tile \"soil_depth\")) (setq soil-phi-str (get_tile \"soil_phi\")) (if (and soil-depth-str (/= soil-depth-str \"\") (numberp (read soil-depth-str))) (progn (setq soil-depth (atof soil-depth-str)) (setq soil-phi (if (numberp (read soil-phi-str)) (atof soil-phi-str) 0.0)) (setq h-pattern (nth 1 selected-type-info)) (setq h-scale (nth 2 selected-type-info)) (setq h-angle (nth 3 selected-type-info)) (setq new-layer (list user-name soil-depth h-pattern h-scale h-angle type-name soil-phi)) (setq i 0) (setq new-list '()) (foreach item temp-layers (if (= i sel-idx) (setq new-list (append new-list (list new-layer))) (setq new-list (append new-list (list item)))) (setq i (1+ i))) (setq temp-layers new-list) (update-soil-list) (set_tile \"soil_list\" (itoa sel-idx))) (alert \"수정할 깊이를 올바르게 입력하세요!\"))) (alert \"수정할 항목을 목록에서 선택하세요!\")))")
        (action_tile "btn_delete" "(progn (setq sel-idx (atoi (get_tile \"soil_list\"))) (if (and temp-layers (>= sel-idx 0) (< sel-idx (length temp-layers))) (progn (setq temp-layers (vl-remove (nth sel-idx temp-layers) temp-layers)) (update-soil-list)) (alert \"삭제할 항목을 선택하세요!\")))")
        (action_tile "btn_clear" "(progn (setq temp-layers '()) (update-soil-list))")
        (action_tile "btn_n_add" "(action-n-add)")
        (action_tile "btn_n_modify" "(action-n-modify)")
        (action_tile "btn_n_delete" "(action-n-delete)")
        (action_tile "btn_n_clear" "(progn (setq temp-n-vals '()) (update-n-val-list) (set_tile \"n_hit\" \"\") (set_tile \"n_pen\" \"\") (set_tile \"n_depth\" \"\"))")
        (action_tile "btn_n_get_dwg" "(progn (setq temp-hole (get_tile \"hole_no\")) (setq *tsp-water-chk* (get_tile \"soil_water_chk\")) (if (= *tsp-water-chk* \"1\") (setq *tsp-water-depth* (atof (get_tile \"soil_water_depth\"))) (setq *tsp-water-depth* 0.0)) (setq *tsp-shotcrete-chk* (get_tile \"soil_shotcrete_chk\")) (if (and (= *tsp-shotcrete-chk* \"1\") current-rock-list (> (length current-rock-list) 0)) (setq *tsp-shotcrete-layer* (nth (atoi (get_tile \"soil_shotcrete_layer\")) current-rock-list))) (done_dialog 4))")
        (action_tile "n_val_list" "(action-n-select)")
        (action_tile "btn_legend" "(progn (setq temp-hole (get_tile \"hole_no\")) (setq *tsp-water-chk* (get_tile \"soil_water_chk\")) (if (= *tsp-water-chk* \"1\") (setq *tsp-water-depth* (atof (get_tile \"soil_water_depth\"))) (setq *tsp-water-depth* 0.0)) (setq *tsp-shotcrete-chk* (get_tile \"soil_shotcrete_chk\")) (if (and (= *tsp-shotcrete-chk* \"1\") current-rock-list (> (length current-rock-list) 0)) (setq *tsp-shotcrete-layer* (nth (atoi (get_tile \"soil_shotcrete_layer\")) current-rock-list))) (done_dialog 3))")
        (action_tile "btn_measure" "(progn (setq temp-hole (get_tile \"hole_no\")) (setq *tsp-water-chk* (get_tile \"soil_water_chk\")) (if (= *tsp-water-chk* \"1\") (setq *tsp-water-depth* (atof (get_tile \"soil_water_depth\"))) (setq *tsp-water-depth* 0.0)) (setq *tsp-shotcrete-chk* (get_tile \"soil_shotcrete_chk\")) (if (and (= *tsp-shotcrete-chk* \"1\") current-rock-list (> (length current-rock-list) 0)) (setq *tsp-shotcrete-layer* (nth (atoi (get_tile \"soil_shotcrete_layer\")) current-rock-list))) (done_dialog 5))")

        ;; [중요] 확인 버튼을 눌렀을 때 체크가 꺼져있으면 정착고 값을 "0"으로 강제 덮어쓰기
        (action_tile "accept" "(progn
          (setq *tsp-max-depth* (atof (get_tile \"soil_max_depth\")))
          (setq *tsp-embed-depth* (atof (get_tile \"soil_embed_depth\")))

          (setq *tsp-rock-embed-chk* (get_tile \"soil_rock_embed_chk\"))
          (if (= *tsp-rock-embed-chk* \"1\")
            (setq *tsp-rock-embed* (get_tile \"soil_rock_embed\"))
            (setq *tsp-rock-embed* \"0\") ;; 체크가 해제되어 있다면 무조건 0 저장
          )

          (setq *tsp-cip-rock-chk* (get_tile \"soil_cip_rock_chk\"))
          (setq *tsp-soil-layers* temp-layers)
          (setq *tsp-n-values* temp-n-vals)
          (setq *tsp-hole-no* (get_tile \"hole_no\"))
          (setq *tsp-water-chk* (get_tile \"soil_water_chk\"))
          (if (= *tsp-water-chk* \"1\") (setq *tsp-water-depth* (atof (get_tile \"soil_water_depth\"))) (setq *tsp-water-depth* 0.0))
          (setq *tsp-shotcrete-enable* (get_tile \"soil_shotcrete_enable\"))
          (setq *tsp-shotcrete-chk* (get_tile \"soil_shotcrete_chk\"))
          (setq *tsp-shotcrete-thick* (atof (get_tile \"soil_shotcrete_thick\")))

          (if (and (= *tsp-shotcrete-chk* \"1\") current-rock-list (> (length current-rock-list) 0))
            (setq *tsp-shotcrete-layer* (nth (atoi (get_tile \"soil_shotcrete_layer\")) current-rock-list))
            (setq *tsp-shotcrete-layer* \"\")
          )
          (if (and (= *tsp-cip-rock-chk* \"1\") current-rock-list (> (length current-rock-list) 0))
            (setq *tsp-cip-rock-layer* (nth (atoi (get_tile \"soil_cip_rock_layer\")) current-rock-list))
            (setq *tsp-cip-rock-layer* \"\")
          )
          (done_dialog 1))")

        (action_tile "back" "(done_dialog 2)")
        (action_tile "cancel" "(done_dialog 0)")
        (setq result (start_dialog))
        (unload_dialog dcl-id)

        ;; (결과 처리 cond 부분 생략 없이 유지)
        (cond
          ((= result 3)
           (setq lg-mode nil pt nil)
           (if (null temp-layers)
             (setq lg-mode "All")
             (progn
               (initget "All Used")
               (setq lg-mode (vl-catch-all-apply 'getkword (list "\n범례 출력 방식을 선택하세요 [전체(A)/사용된지층(U)] <All>: ")))
               (if (vl-catch-all-error-p lg-mode) (setq lg-mode nil) (if (null lg-mode) (setq lg-mode "All")))
             )
           )
           (if lg-mode
             (progn
               (setq pt (vl-catch-all-apply 'getpoint (list "\n범례를 삽입할 기준점(좌측상단)을 클릭하세요: ")))
               (if (vl-catch-all-error-p pt) (setq pt nil))
             )
           )
           (if (and lg-mode pt)
             (progn (setq leg-box (draw-tsp-legend pt lg-mode temp-layers)) (if leg-box (tsp-zoom-legend leg-box)))
             (princ "\n범례 생성이 취소되었습니다. 지반 정의 창으로 되돌아갑니다.")
           )
          )
          ((= result 4)
           (setq extracted-n-vals (tsp-get-nvalue-from-drawing))
           (if extracted-n-vals
             (progn
               (setq temp-n-vals (append temp-n-vals extracted-n-vals))
               (setq temp-n-vals (vl-sort temp-n-vals '(lambda (a b) (< (car a) (car b)))))
             )
           )
          )
          ((= result 5)
           (if tsp-pause-for-measurement
             (tsp-pause-for-measurement)
             (alert "TSP - 도면 제어 모듈이 로드되지 않았습니다.")
           )
          )
          (t (setq loop-dcl nil))
        )
      )
      (setq loop-dcl nil)
    )
  )
  result
)
