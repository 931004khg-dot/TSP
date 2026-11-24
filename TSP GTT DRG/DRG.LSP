
;; =========================================================================
;; TEP : 텍스트 객체에서 표고(EL) 숫자를 추출하여 3D Point 및 표고 문자를 일괄 생성
;; =========================================================================

(defun c:TEP ( / old_osmode old_err txt_raw_str res in_number i len char z_val 
               sty_sel sty_ent sty_data saved_hgt saved_sty use_hgt use_sty defer_style
               z_sel z_ent z_data curr_hgt curr_sty ins_pt pt3d )
  (vl-load-com)
  
  ;; 백그라운드 도면층 생성 함수
  (defun create_layer (lyr col)
    (if (not (tblsearch "LAYER" lyr))
      (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") (cons 2 lyr) '(70 . 0) (cons 62 col)))
    )
  )

  ;; 에러 처리 (안전장치)
  (setq old_err *error*)
  (defun *error* (msg)
    (setq *error* old_err)
    (princ "\n작업이 취소되었습니다.")
    (princ)
  )

  ;; _LEVL 도면층 생성
  (create_layer "_LEVL" 7)

  ;; =========================================================================
  ;; [STEP 1] 텍스트 스타일 및 높이 결정 (1~3순위 로직)
  ;; =========================================================================
  (setq sty_sel (entsel "\n텍스트 높이와 스타일을 가져올 문자 선택 (엔터: 도면 저장값 또는 Z값 문자 속성 사용): "))
  (setq defer_style nil use_hgt nil use_sty nil)

  (if sty_sel
    (progn
      ;; 1순위: 직접 클릭한 경우 -> 속성 추출 후 도면에 영구 저장 (LData)
      (setq sty_ent (car sty_sel) sty_data (entget sty_ent))
      (if (member (cdr (assoc 0 sty_data)) '("TEXT" "MTEXT"))
        (progn
          (setq use_hgt (cdr (assoc 40 sty_data)) use_sty (cdr (assoc 7 sty_data)))
          (vlax-ldata-put "TEP_SETTINGS" "TXT_HGT" use_hgt)
          (vlax-ldata-put "TEP_SETTINGS" "TXT_STY" use_sty)
          (princ (strcat "\n-> 1순위: 선택한 객체의 속성을 적용 및 저장했습니다. (높이: " (rtos use_hgt 2 2) ", 스타일: " use_sty ")"))
        )
        (progn (princ "\n문자가 아닙니다. 2순위/3순위 로직으로 넘어갑니다.") (setq sty_sel nil))
      )
    )
  )

  (if (null sty_sel) ; 텍스트를 선택하지 않고 엔터를 친 경우
    (progn
      ;; 2순위: 도면에 저장된 LData가 있는지 확인
      (setq saved_hgt (vlax-ldata-get "TEP_SETTINGS" "TXT_HGT"))
      (setq saved_sty (vlax-ldata-get "TEP_SETTINGS" "TXT_STY"))
      (if (and saved_hgt saved_sty)
        (progn
          (setq use_hgt saved_hgt use_sty saved_sty)
          (princ (strcat "\n-> 2순위: 도면에 저장된 이전 속성을 불러옵니다. (높이: " (rtos use_hgt 2 2) ", 스타일: " use_sty ")"))
        )
        (progn
          ;; 3순위: 저장된 데이터도 없으면 Z값 문자의 속성을 따르도록 플래그 설정
          (setq defer_style T)
          (princ "\n-> 3순위: 저장된 속성이 없습니다. 이후 클릭할 Z값 문자의 속성을 임시로 사용합니다.")
        )
      )
    )
  )

  ;; =========================================================================
  ;; [STEP 2] Z값 텍스트 선택 루프 (바깥쪽 루프)
  ;; =========================================================================
  (while (setq z_sel (entsel "\nEL값을 추출할 문자 선택 : "))
    (setq z_ent (car z_sel) z_data (entget z_ent))
    
    (if (member (cdr (assoc 0 z_data)) '("TEXT" "MTEXT"))
      (progn
        (setq txt_raw_str (cdr (assoc 1 z_data)))
        
        ;; 스마트 숫자 추출 로직
        (setq res "" in_number nil i 1 len (strlen txt_raw_str))
        (while (and (<= i len) (not in_number))
          (setq char (substr txt_raw_str i 1))
          (if (wcmatch char "[0-9]")
            (progn
              (setq in_number T)
              (if (and (> i 1) (= (substr txt_raw_str (1- i) 1) "-"))
                (setq res (strcat "-" char))
                (setq res char)
              )
            )
          )
          (setq i (1+ i))
        )
        (while (and (<= i len) in_number)
          (setq char (substr txt_raw_str i 1))
          (if (wcmatch char "[0-9.]")
            (setq res (strcat res char))
            (setq in_number nil)
          )
          (setq i (1+ i))
        )
        
        (if (= res "")
          (princ "\n텍스트 내에서 숫자를 찾을 수 없습니다. 다시 선택해주세요.")
          (progn
            ;; 추출된 숫자를 소수점 2째자리로 확정
            (setq z_val (atof (rtos (atof res) 2 2)))
            
            ;; 3순위 플래그 확인: defer_style이 참이면 현재 Z값 문자의 속성 적용
            (if defer_style
              (progn
                (setq curr_hgt (cdr (assoc 40 z_data)) curr_sty (cdr (assoc 7 z_data)))
              )
              (progn
                (setq curr_hgt use_hgt curr_sty use_sty)
              )
            )

            ;; =========================================================================
            ;; [STEP 3] 포인트 및 텍스트 삽입 루프 (안쪽 루프)
            ;; =========================================================================
            (princ (strcat "\n현재 저장된 EL값: " (rtos z_val 2 2)))
            (while (setq ins_pt (getpoint "\n위치 클릭 (엔터: 새로운 EL 문자 선택): "))
              (setq pt3d (list (car ins_pt) (cadr ins_pt) z_val))
              
              ;; 3D Point 생성
              (entmake (list '(0 . "POINT") (cons 10 pt3d) '(8 . "_LEVL") '(62 . 256)))
              ;; 텍스트 생성
              (entmake (list '(0 . "TEXT") (cons 10 pt3d) (cons 40 curr_hgt) (cons 1 (rtos z_val 2 2)) (cons 7 curr_sty) '(8 . "_LEVL") '(62 . 256)))
            )
          )
        )
      )
      (princ "\n선택한 객체가 문자가 아닙니다. 다시 선택해주세요.")
    )
  )

  (setq *error* old_err)
  (princ "\n명령이 종료되었습니다.")
  (princ)
)
(princ "\n명령어 DRG, TEP 가 로드되었습니다.")
(princ)

;; =========================================================================
;; DRG : 평면 선형(2D 폴리선)과 3D 삼각망/Z포인트를 기반으로 종단면도 자동 생성 및 넘버링
;; =========================================================================

(defun c:DRG ( / old_osmode old_dyn old_prm
               ent_sel entPline pick_pt pline_data pt_list start_pt_dist end_pt_dist total_pline_dist
               run_mode p_sub_mode loop_edit t_sel1 t_ent1 t_data1 t_sel2 t_ent2 t_data2 ref_val_str ref_val
               t_pt t_lay pt_x pt_y tol p_ss p_ent p_data
               ssPts gubae pt_data_list p_idx e_pt pt_coord cpt dist_to_cpt chain
               ssTIN idx entTIN tin_data t1 t2 t3 t4 tin_list
               is_valid result_list z_val curr_tri nx ny nz v1 v2 tmp_pt dist p1
               txt_sel txt_ent txt_data txt_sty txt_hgt
               scl kw insPt x_draw y_draw horiz_start horiz_end t_idx ent_list
               base_pt top_pt pt3d z_str txt_pt base_chain profile_len
               i prev_item cur_item cur_x cur_y prev_y vert_count
               valid_segs j cur_seg prev_seg next_seg
               final_segs cliff_jumps x_start x_end y_cur y_prev y_next
               final_start_x final_end_x modified_start modified_end dx
               p_cur p_next cad_x1 cad_x2 cad_y cad_x center_x bx by dir vx vy u_x u_y draw_len pt_start pt_end 
               param deriv ang perp line_end circle_center p_rad p_line_len max_vert_hgt tmp_h target_h prof_num_y num_pt 
               param_prev param_next d1 d2 a1 a2 v_sum angle_dir text_angle p_line_start p_line_end
               restore_sys create_layer sign is-pt-in-tri get-z len1 len2 v_in v_out v_away1 
               leader_bottom_y leader_top_y circle_y check-collision final_data num_verts evade_angles 
               prev_idx next_idx orig_angle_dir test_idx collision evade_sign )
  (vl-load-com)

  (setq old_dyn (getvar "DYNMODE"))
  (setq old_prm (getvar "DYNPROMPT"))
  (setvar "DYNMODE" 3)
  (setvar "DYNPROMPT" 1)

  (defun restore_sys ()
    (if old_dyn (setvar "DYNMODE" old_dyn))
    (if old_prm (setvar "DYNPROMPT" old_prm))
  )

  (defun create_layer (lyr col)
    (if (not (tblsearch "LAYER" lyr))
      (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") (cons 2 lyr) '(70 . 0) (cons 62 col)))
    )
  )

  ;;;;==========================================================================
  ;;;; 공통 내부 함수 정의 (M모드 및 E모드 전용 데이터 보간)
  ;;;;==========================================================================
  (defun get-pline-vertices-x (pline_ent / pts k end_p p pline_data)
    (setq pts nil pline_data (entget pline_ent))
    (if (= (cdr (assoc 0 pline_data)) "LWPOLYLINE")
      (progn
        (setq k 0.0 end_p (vlax-curve-getEndParam pline_ent))
        (while (<= k end_p)
          (setq p (vlax-curve-getPointAtParam pline_ent k))
          (if p (setq pts (append pts (list (car p)))))
          (setq k (1+ k))
        )
      )
      (if (= (cdr (assoc 0 pline_data)) "LINE")
        (setq pts (list (car (cdr (assoc 10 pline_data))) (car (cdr (assoc 11 pline_data)))))
      )
    )
    pts
  )

  (defun get-y-on-pline (pline_ent target_x / pts k end_p p1 p2 y_val min_x max_x pline_data)
    (setq pts nil pline_data (entget pline_ent))
    (if (= (cdr (assoc 0 pline_data)) "LWPOLYLINE")
      (progn
        (setq k 0.0 end_p (vlax-curve-getEndParam pline_ent))
        (while (<= k end_p)
          (setq pts (append pts (list (vlax-curve-getPointAtParam pline_ent k))))
          (setq k (1+ k))
        )
      )
      (if (= (cdr (assoc 0 pline_data)) "LINE")
        (setq pts (list (cdr (assoc 10 pline_data)) (cdr (assoc 11 pline_data))))
      )
    )
    (setq y_val nil k 0)
    (while (and (< k (1- (length pts))) (null y_val))
      (setq p1 (nth k pts) p2 (nth (1+ k) pts))
      (setq min_x (min (car p1) (car p2)) max_x (max (car p1) (car p2)))
      (if (and (>= target_x (- min_x 1.0)) (<= target_x (+ max_x 1.0)))
        (if (< (abs (- (car p2) (car p1))) 1e-4)
          (setq y_val (max (cadr p1) (cadr p2)))
          (setq y_val (+ (cadr p1) (* (- (cadr p2) (cadr p1)) (/ (- target_x (car p1)) (- (car p2) (car p1))))))
        )
      )
      (setq k (1+ k))
    )
    y_val
  )

  ;;;;==========================================================================
  ;;;; 메인 옵션 선택 : 종단작성 / 최대깊이산정 / 전개도작성
  ;;;;==========================================================================
  (initget "D M E")
  (setq main_opt (getkword "\n작업 선택 [종단작성(D) / 최대깊이산정(M) / 전개도작성(E)] <D>: "))
  (if (null main_opt) (setq main_opt "D"))

  (cond
    ((= main_opt "D")
    (progn ; [종단작성 모드 시작]
      ;; 허공 클릭 시 종료되지 않고 재선택을 요구하는 루프 적용
      (setq ent_sel nil)
      (while (null ent_sel)
        (setq ent_sel (entsel "\n기준 선형을 선택하세요 (클릭한 지점이 시점이 됩니다): "))
        (if (null ent_sel) (princ "\n선형이 선택되지 않았습니다. 다시 선택해주세요."))
      )
      (setq entPline (car ent_sel))
      (setq pick_pt (cadr ent_sel))
      (setq pline_data (entget entPline))
      
      (if (/= (cdr (assoc 0 pline_data)) "LWPOLYLINE")
        (progn (alert "2D 폴리선(LWPOLYLINE)만 선택 가능합니다.") (restore_sys) (exit))
      )

      (setq total_pline_dist (vlax-curve-getDistAtParam entPline (vlax-curve-getEndParam entPline)))
      
      ;; D모드 폐합 여부 검사 및 수동 시점/방향 입력 로직
      (setq is_closed_d (vlax-curve-isClosed entPline))
      (if is_closed_d
        (progn
          (setq manual_start_pt_d (getpoint "\n폐합된 선형입니다. 평면도에서 종단면도의 0m 시점으로 사용할 정확한 시작점을 클릭하세요: "))
          (setq manual_dir_pt_d (getpoint manual_start_pt_d "\n종단면도 진행 방향에 있는 선형 위의 임의의 점을 클릭하세요: "))
          
          (setq base_start_param_d (vlax-curve-getParamAtPoint entPline (vlax-curve-getClosestPointTo entPline manual_start_pt_d)))
          (setq base_start_dist_d (vlax-curve-getDistAtParam entPline base_start_param_d))
          
          (setq dir_param_d (vlax-curve-getParamAtPoint entPline (vlax-curve-getClosestPointTo entPline manual_dir_pt_d)))
          (setq dir_dist_d (vlax-curve-getDistAtParam entPline dir_param_d))
          
          (setq dist_diff_d (- dir_dist_d base_start_dist_d))
          (if (< dist_diff_d 0) (setq dist_diff_d (+ total_pline_dist dist_diff_d)))
          (if (< dist_diff_d (/ total_pline_dist 2.0))
            (setq is_forward_d T)
            (setq is_forward_d nil)
          )
        )
        ;; 열린 선형 거리 계산 구문
        (progn
          (setq total_pline_dist (vlax-curve-getDistAtParam entPline (vlax-curve-getEndParam entPline)))
          (setq start_pt_dist (distance (vlax-curve-getStartPoint entPline) pick_pt))
          (setq end_pt_dist (distance (vlax-curve-getEndPoint entPline) pick_pt))
        )
      )

  ;; LData를 활용한 텍스트 속성 저장 및 불러오기 로직 (D모드)
  (setq txt_hgt nil txt_sty nil loop_txt T)
  (while loop_txt
    (setq txt_sel (entsel "\n넘버링 및 종단면에 사용할 텍스트 높이와 스타일을 위해 문자 객체를 선택하세요 (엔터: 도면 저장값 사용): "))
    (if txt_sel
      (progn
        (setq txt_ent (car txt_sel) txt_data (entget txt_ent))
        (if (member (cdr (assoc 0 txt_data)) '("TEXT" "MTEXT"))
          (progn
            (setq txt_sty (cdr (assoc 7 txt_data)) txt_hgt (cdr (assoc 40 txt_data)))
            (vlax-ldata-put "DRG_SETTINGS" "TXT_HGT" txt_hgt)
            (vlax-ldata-put "DRG_SETTINGS" "TXT_STY" txt_sty)
            (princ (strcat "\n-> 선택한 객체의 속성을 적용 및 저장했습니다. (높이: " (rtos txt_hgt 2 2) ", 스타일: " txt_sty ")"))
            (setq loop_txt nil)
          )
          (princ "\n문자가 아닙니다. 다시 선택해주세요.")
        )
      )
      (progn
        (setq saved_hgt (vlax-ldata-get "DRG_SETTINGS" "TXT_HGT"))
        (setq saved_sty (vlax-ldata-get "DRG_SETTINGS" "TXT_STY"))
        (if (and saved_hgt saved_sty)
          (progn
            (setq txt_hgt saved_hgt txt_sty saved_sty)
            (princ (strcat "\n-> 도면에 저장된 이전 속성을 불러옵니다. (높이: " (rtos txt_hgt 2 2) ", 스타일: " txt_sty ")"))
            (setq loop_txt nil)
          )
          (princ "\n저장된 텍스트 속성이 없습니다. 텍스트 객체를 선택해주세요.")
        )
      )
    )
  )

  (initget "T C P")
  (setq run_mode (getkword "\n종단 생성 방식 선택 [3D삼각망(T) / 등고선(C) / Z포인트(P)] <T>: "))
  (if (null run_mode) (setq run_mode "T"))

  ;; P 방식일 경우 내부 1번/2번 분기 적용 (마우스 클릭 대응)
  (if (= run_mode "P")
    (progn
      ;; 마우스로 텍스트 자체를 클릭했을 때 넘어오는 문자열도 허용하도록 키워드 추가
      (initget "1 1:지표면종단 2 2:굴착바닥고")
      (setq p_sub_mode (getkword "\nZ포인트 종단 종류 선택 [1:지표면종단 / 2:굴착바닥고] <1>: "))
      
      (cond
        ((or (null p_sub_mode) (= p_sub_mode "1") (= p_sub_mode "1:지표면종단")) (setq p_sub_mode "1"))
        ((or (= p_sub_mode "2") (= p_sub_mode "2:굴착바닥고")) (setq p_sub_mode "2"))
        (t (setq p_sub_mode "1"))
      )
      (setq run_mode (strcat "P" p_sub_mode)) ; "P1" 또는 "P2" 로 설정됨
    )
  )

  ;; =========================================================================
  ;; [1번 분기 전용] Z값 사전 수정 모듈
  ;; =========================================================================
  (if (= run_mode "P1")
    (progn
      (setq loop_edit T)
      (princ "\n\n*** [레벨 수정 모드] ***")
      (while loop_edit
        (setq t_sel1 (entsel "\n수정할 대상 텍스트 선택 (엔터입력 시 다음단계 진행): "))
        (if t_sel1
          (progn
            (setq t_ent1 (car t_sel1) t_data1 (entget t_ent1))
            (if (member (cdr (assoc 0 t_data1)) '("TEXT" "MTEXT"))
              (progn
                (setq t_sel2 (entsel "\n가져올(참조할) 텍스트 선택: "))
                (if t_sel2
                  (progn
                    (setq t_ent2 (car t_sel2) t_data2 (entget t_ent2))
                    (if (member (cdr (assoc 0 t_data2)) '("TEXT" "MTEXT"))
                      (progn
                        ;; 참조할 값 추출
                        (setq ref_val_str (cdr (assoc 1 t_data2)))
                        (setq ref_val (atof ref_val_str))
                        
                        ;; 대상 텍스트 수정 및 노란색 변경
                        (setq t_data1 (subst (cons 1 ref_val_str) (assoc 1 t_data1) t_data1))
                        (if (assoc 62 t_data1)
                          (setq t_data1 (subst '(62 . 2) (assoc 62 t_data1) t_data1))
                          (setq t_data1 (append t_data1 (list '(62 . 2))))
                        )
                        (entmod t_data1)
                        
                        ;; Point 객체 추적 (XY 좌표 기준)
                        (setq t_pt (cdr (assoc 10 t_data1)) t_lay (cdr (assoc 8 t_data1)))
                        (setq pt_x (car t_pt) pt_y (cadr t_pt))
                        (setq tol 1e-4) ;; 캐드 부동소수점 오차 허용 범위
                        (setq p_ss (ssget "C" (list (- pt_x tol) (- pt_y tol)) (list (+ pt_x tol) (+ pt_y tol)) '((0 . "POINT"))))
                        
                        (if p_ss
                          (progn
                            ;; 동일 좌표의 Point가 존재할 경우 업데이트
                            (setq p_ent (ssname p_ss 0) p_data (entget p_ent))
                            (setq p_data (subst (cons 10 (list pt_x pt_y ref_val)) (assoc 10 p_data) p_data))
                            (if (assoc 62 p_data)
                              (setq p_data (subst '(62 . 2) (assoc 62 p_data) p_data))
                              (setq p_data (append p_data (list '(62 . 2))))
                            )
                            (entmod p_data)
                            (princ (strcat "\n-> 기존 Point의 Z값을 " ref_val_str "로 수정 완료."))
                          )
                          (progn
                            ;; 동일 좌표에 Point가 없을 경우 신규 생성
                            (entmake (list '(0 . "POINT") (cons 10 (list pt_x pt_y ref_val)) (cons 8 t_lay) '(62 . 2)))
                            (princ (strcat "\n-> Point가 없어 새로 생성했습니다. (Z: " ref_val_str ")"))
                          )
                        )
                      )
                      (princ "\n참조 객체가 문자가 아닙니다.")
                    )
                  )
                  (princ "\n가져올 텍스트를 선택하지 않았습니다.")
                )
              )
              (princ "\n선택한 객체가 문자가 아닙니다.")
            )
          )
          (setq loop_edit nil) ; 엔터 시 루프 종료
        )
      )
    )
  )
  ;; =========================================================================

  (setq result_list nil is_valid T)

  (cond
    ((= run_mode "T")
    (progn
      (if is_closed_d
        (progn
          ;; 폐합 선형: 지정한 0m 시점 기준으로 점(Vertex)들을 거리순으로 재정렬
          (setq tmp_list nil)
          (foreach item pline_data
            (if (= (car item) 10)
              (progn
                (setq cpt (vlax-curve-getClosestPointTo entPline (cdr item)))
                (setq cur_dist (vlax-curve-getDistAtPoint entPline cpt))
                (if is_forward_d
                  (setq chain (- cur_dist base_start_dist_d))
                  (setq chain (- base_start_dist_d cur_dist))
                )
                (if (< (abs chain) 0.01) (setq chain 0.0))
                (if (< chain -0.01) (setq chain (+ total_pline_dist chain)))
                (setq tmp_list (append tmp_list (list (list chain (cdr item)))))
              )
            )
          )
          (setq tmp_list (vl-sort tmp_list '(lambda (a b) (< (car a) (car b)))))
          (setq pt_list nil)
          (foreach item tmp_list (setq pt_list (append pt_list (list (cadr item)))))
        )
        (progn
          ;; 열린 선형: 기존 방식
          (setq pt_list nil)
          (foreach item pline_data
            (if (= (car item) 10) (setq pt_list (append pt_list (list (cdr item)))))
          )
          (if (> start_pt_dist end_pt_dist) (setq pt_list (reverse pt_list)))
        )
      )

      (princ "\nZ값을 추출할 3D 삼각망(3DFACE) 객체들을 선택하세요:")
      (setq ssTIN (ssget '((0 . "3DFACE"))))
      (if (null ssTIN)
        (progn (princ "\n삼각망이 선택되지 않았습니다.") (restore_sys) (exit))
      )

      (setq tin_list nil idx 0)
      (while (< idx (sslength ssTIN))
        (setq entTIN (ssname ssTIN idx) tin_data (entget entTIN))
        (setq t1 (cdr (assoc 10 tin_data)) t2 (cdr (assoc 11 tin_data)) t3 (cdr (assoc 12 tin_data)) t4 (cdr (assoc 13 tin_data)))
        (setq tin_list (append tin_list (list (list t1 t2 t3))))
        (if (not (equal t3 t4 1e-8)) (setq tin_list (append tin_list (list (list t1 t3 t4)))))
        (setq idx (1+ idx))
      )

      (defun sign (p1 p2 p3) (- (* (- (car p1) (car p3)) (- (cadr p2) (cadr p3))) (* (- (car p2) (car p3)) (- (cadr p1) (cadr p3)))))
      (defun is-pt-in-tri (pt v1 v2 v3 / d1 d2 d3)
        (setq d1 (sign pt v1 v2) d2 (sign pt v2 v3) d3 (sign pt v3 v1))
        (not (and (or (< d1 -1e-8) (< d2 -1e-8) (< d3 -1e-8)) (or (> d1 1e-8) (> d2 1e-8) (> d3 1e-8))))
      )
      (defun get-z (pt p1 p2 p3 / v1 v2 nx ny nz)
        (setq v1 (list (- (car p2) (car p1)) (- (cadr p2) (cadr p1)) (- (caddr p2) (caddr p1))))
        (setq v2 (list (- (car p3) (car p1)) (- (cadr p3) (cadr p1)) (- (caddr p3) (caddr p1))))
        (setq nx (- (* (cadr v1) (caddr v2)) (* (caddr v1) (cadr v2))))
        (setq ny (- (* (caddr v1) (car v2)) (* (car v1) (caddr v2))))
        (setq nz (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2))))
        (if (equal nz 0.0 1e-8) nil (- (caddr p1) (/ (+ (* nx (- (car pt) (car p1))) (* ny (- (cadr pt) (cadr p1)))) nz)))
      )

      (setq chain 0.0 idx 0)
      (while (and (< idx (length pt_list)) is_valid)
        (setq tmp_pt (nth idx pt_list))
        (if (= idx 0) (setq dist 0.0)
          (setq p1 (nth (1- idx) pt_list) dist (distance p1 tmp_pt) chain (+ chain dist))
        )
        (setq z_val nil t_idx 0)
        (while (and (null z_val) (< t_idx (length tin_list)))
          (setq curr_tri (nth t_idx tin_list))
          (if (is-pt-in-tri tmp_pt (car curr_tri) (cadr curr_tri) (caddr curr_tri))
            (setq z_val (get-z tmp_pt (car curr_tri) (cadr curr_tri) (caddr curr_tri)))
          )
          (setq t_idx (1+ t_idx))
        )
        (if z_val
          (setq result_list (append result_list (list (list chain (atof (rtos z_val 2 2)) tmp_pt))))
          (setq is_valid nil) 
        )
        (setq idx (1+ idx))
      )
    )
    )
    ;; =========================================================================
    ;; [등고선(C) 모드 전용 로직 추가]
    ;; =========================================================================
    ((= run_mode "C")
      (progn
        ;; 1. 선형의 기본 절점(Vertex)과 해당 누적 거리(Chain) 추출
        (setq pt_list_with_chain nil)
        (if is_closed_d
          (progn
            (setq tmp_list nil)
            (foreach item pline_data
              (if (= (car item) 10)
                (progn
                  (setq cpt (vlax-curve-getClosestPointTo entPline (cdr item)))
                  (setq cur_dist (vlax-curve-getDistAtPoint entPline cpt))
                  (if is_forward_d
                    (setq chain (- cur_dist base_start_dist_d))
                    (setq chain (- base_start_dist_d cur_dist))
                  )
                  (if (< (abs chain) 0.01) (setq chain 0.0))
                  (if (< chain -0.01) (setq chain (+ total_pline_dist chain)))
                  (setq tmp_list (append tmp_list (list (list chain (cdr item)))))
                )
              )
            )
            (setq tmp_list (vl-sort tmp_list '(lambda (a b) (< (car a) (car b)))))
            (setq pt_list_with_chain tmp_list)
          )
          (progn
            (foreach item pline_data
              (if (= (car item) 10) 
                (progn
                  (setq cpt (vlax-curve-getClosestPointTo entPline (cdr item)))
                  (setq chain (vlax-curve-getDistAtPoint entPline cpt))
                  (if (> start_pt_dist end_pt_dist) (setq chain (- total_pline_dist chain)))
                  (setq pt_list_with_chain (append pt_list_with_chain (list (list chain (cdr item)))))
                )
              )
            )
            (if (> start_pt_dist end_pt_dist) 
              (setq pt_list_with_chain (vl-sort pt_list_with_chain '(lambda (a b) (< (car a) (car b)))))
            )
          )
        )

        ;; 2. 등고선 선택 및 모든 교차점(실제 레벨 데이터) 추출
        (princ "\n고도(Elevation) 속성을 가진 등고선(LWPOLYLINE) 객체들을 선택하세요:")
        (setq ssContour (ssget '((0 . "LWPOLYLINE"))))
        (if (null ssContour)
          (progn (princ "\n등고선이 선택되지 않았습니다.") (restore_sys) (exit))
        )
        
        (setq intersect_list nil idx 0)
        (setq vla_entPline (vlax-ename->vla-object entPline))
        (setq pline_elev (vlax-get-property vla_entPline 'Elevation))
        
        (while (< idx (sslength ssContour))
          (setq entContour (ssname ssContour idx))
          (setq vla_contour (vlax-ename->vla-object entContour))
          (setq contour_elev (vlax-get-property vla_contour 'Elevation))
          
          ;; Z값 단차로 인한 교차점 인식 실패 방지
          (vlax-put-property vla_contour 'Elevation pline_elev)
          (setq p_list (vlax-invoke vla_entPline 'IntersectWith vla_contour 0))
          (vlax-put-property vla_contour 'Elevation contour_elev) ;; 원상복구
          
          (if p_list
            (progn
              (setq i 0)
              (while (< i (length p_list))
                (setq i_pt (list (nth i p_list) (nth (1+ i) p_list) (nth (+ i 2) p_list)))
                (setq cpt (vlax-curve-getClosestPointTo entPline i_pt))
                (setq cur_dist (vlax-curve-getDistAtPoint entPline cpt))
                
                (if is_closed_d
                  (progn
                    (if is_forward_d
                      (setq i_chain (- cur_dist base_start_dist_d))
                      (setq i_chain (- base_start_dist_d cur_dist))
                    )
                    (if (< (abs i_chain) 0.01) (setq i_chain 0.0))
                    (if (< i_chain -0.01) (setq i_chain (+ total_pline_dist i_chain)))
                  )
                  (progn
                    (setq i_chain cur_dist)
                    (if (> start_pt_dist end_pt_dist) (setq i_chain (- total_pline_dist i_chain)))
                  )
                )
                (setq pt3d (list (car i_pt) (cadr i_pt) contour_elev))
                ;; [핵심] 교차점 리스트에 누적거리, 고도, 3D좌표 모두 저장
                (setq intersect_list (append intersect_list (list (list i_chain contour_elev pt3d))))
                (setq i (+ i 3))
              )
            )
          )
          (setq idx (1+ idx))
        )
        
        (if (null intersect_list)
          (progn (alert "선형과 교차하는 등고선이 없습니다.") (restore_sys) (exit))
        )
        
        ;; 3. 교차점을 거리순으로 정렬
        (setq intersect_list (vl-sort intersect_list '(lambda (a b) (< (car a) (car b)))))
        
        ;; 4. 기존 선형 절점들의 Z값을 등고선 데이터 기반으로 보간
        (setq temp_vert_list nil)
        (foreach v_item pt_list_with_chain
          (setq v_chain (car v_item) v_pt (cadr v_item))
          (setq z_val nil)
          (setq first_int (nth 0 intersect_list))
          (setq last_int (last intersect_list))
          
          (if (<= v_chain (car first_int))
            (setq z_val (cadr first_int))
            (if (>= v_chain (car last_int))
              (setq z_val (cadr last_int))
              (progn
                (setq int_i 0 found nil)
                (while (and (< int_i (1- (length intersect_list))) (not found))
                  (setq int1 (nth int_i intersect_list) int2 (nth (1+ int_i) intersect_list))
                  (if (and (>= v_chain (car int1)) (<= v_chain (car int2)))
                    (progn
                      (if (= (car int1) (car int2))
                        (setq z_val (cadr int1))
                        (setq z_val (+ (cadr int1) (* (- (cadr int2) (cadr int1)) (/ (- v_chain (car int1)) (- (car int2) (car int1))))))
                      )
                      (setq found T)
                    )
                  )
                  (setq int_i (1+ int_i))
                )
              )
            )
          )
          (setq pt3d (list (car v_pt) (cadr v_pt) z_val))
          (setq temp_vert_list (append temp_vert_list (list (list v_chain z_val pt3d))))
        )

        ;; 5. 등고선 교차점과 선형 보간 절점을 하나로 병합 후 거리순 재정렬
        (setq combined_list (append intersect_list temp_vert_list))
        (setq combined_list (vl-sort combined_list '(lambda (a b) (< (car a) (car b)))))
        
        ;; 6. 중복점 제거 및 최종 result_list 할당
        (setq prev_chain -1e9)
        (foreach item combined_list
          (if (> (abs (- (car item) prev_chain)) 1e-3)
            (progn
              (setq final_z (atof (rtos (cadr item) 2 2)))
              (setq result_list (append result_list (list (list (car item) final_z (caddr item)))))
              (setq prev_chain (car item))
            )
          )
        )
      )
    )
    (t ;; run_mode가 "P1" 이거나 "P2" 일 때 공통 실행
      (progn
      (princ "\n선형 위의 높이점(POINT)을 선택하세요:")
      (setq ssPts (ssget '((0 . "POINT"))))
      (if (null ssPts) (progn (princ "\n포인트가 선택되지 않았습니다.") (restore_sys) (exit)))
      
            (if (= run_mode "P2")
        (progn
          ;; 사용자에게 진행방향 기준 일괄 앞구간 차감 방식임을 안내
          (princ "\n[안내] 굴착바닥고 단차 구배는 선형 진행방향 기준 '일괄 앞구간(이전 구간) 끝점 차감' 방식으로 적용됩니다.")
          (setq gubae (getreal "\n터파기선 구배 입력 1:[ ] <1.2>: "))
          (if (null gubae) (setq gubae 1.2))
        )
      )

      (setq pt_data_list nil p_idx 0)
      (while (< p_idx (sslength ssPts))
        (setq e_pt (ssname ssPts p_idx) pt_coord (cdr (assoc 10 (entget e_pt))))
        (setq cpt (vlax-curve-getClosestPointTo entPline pt_coord))
        (setq dist_to_cpt (distance (list (car pt_coord) (cadr pt_coord)) (list (car cpt) (cadr cpt))))
        (if (< dist_to_cpt 1e-2)
          (progn
            ;; [에러 수정: 폐합선 방향 계산을 위해 현재 포인트의 선형상 투영점 거리(cur_dist) 할당]
            (setq cur_dist (vlax-curve-getDistAtPoint entPline cpt))
            (if is_closed_d
              (progn
                (if is_forward_d
                  (setq chain (- cur_dist base_start_dist_d))
                  (setq chain (- base_start_dist_d cur_dist))
                )
                (if (< (abs chain) 0.01) (setq chain 0.0))
                (if (< chain -0.01) (setq chain (+ total_pline_dist chain)))
              )
              (progn
                (setq chain (vlax-curve-getDistAtPoint entPline cpt))
                (if (> start_pt_dist end_pt_dist) (setq chain (- total_pline_dist chain)))
              )
            )
            (setq pt_data_list (append pt_data_list (list (list chain (atof (rtos (caddr pt_coord) 2 2)) pt_coord))))
          )
        )
        (setq p_idx (1+ p_idx))
      )
      (setq result_list (vl-sort pt_data_list '(lambda (a b) (< (car a) (car b)))))
          
          ;; 절점(Vertex) 위치에 포인트가 2개 이상 있는지 확인하고 에러 메시지 출력
          (setq seen_verts nil)
          (foreach item result_list
            (setq tmp_pt (caddr item))
            (setq param_chk (vlax-curve-getParamAtPoint entPline (vlax-curve-getClosestPointTo entPline tmp_pt)))
            (if param_chk
              (progn
                (setq v_idx (fix (+ param_chk 0.5)))
                (setq v_pt (vlax-curve-getPointAtParam entPline v_idx))
                ;; 파라미터가 아닌 2D 물리적 거리(X, Y)로 절점 일치 여부 확인 (오차 1e-3)
                (if (and v_pt (< (distance (list (car tmp_pt) (cadr tmp_pt)) (list (car v_pt) (cadr v_pt))) 1e-3))
                  (if (member v_idx seen_verts)
                    (progn
                      ;; 중복 발견 시 사용자에게 명확히 위치를 알림 후 프로그램 종료
                      (alert (strcat "\n오류: " (itoa (1+ v_idx)) "번째 절점(모서리)에 포인트(POINT) 객체가 2개 이상 겹쳐 있습니다.\n도면을 확인하여 중복된 포인트를 하나만 남기고 삭제해주세요."))
                      (restore_sys)
                      (exit)
                    )
                    ;; 중복이 아니면 기록 리스트에 추가
                    (setq seen_verts (append seen_verts (list v_idx)))
                  )
                )
              )
            )
          )

          (if (< (length result_list) 2)
            (progn (alert "선형 위에서 유효한 포인트가 2개 이상 발견되지 않았습니다.") (restore_sys) (exit))
          )
        )
      )
    )

      ;; =====================================================================
      ;; 폐합선(Closed) 종단 마감 처리: 0m 시점의 높이 데이터를 맨 마지막(전체 길이)에 복사하여 닫아줌
      ;; =====================================================================
      (if (and is_closed_d (> (length result_list) 0))
        (progn
          (setq first_item (nth 0 result_list))
          ;; 첫 번째 점의 Z값과 3D 좌표는 유지하되, 거리를 전체 길이(total_pline_dist)로 덮어씌움
          (setq end_item (list total_pline_dist (cadr first_item) (caddr first_item)))
          (setq result_list (append result_list (list end_item)))
        )
      )

      (if (not is_valid)
        (progn (alert "선형이 삼각망을 벗어났습니다.") (restore_sys) (exit))
        (progn
          ;; =====================================================================
          ;; 평면도 절점 넘버링
          ;; =====================================================================
      (create_layer "_joint" 1)
      (setq p_rad (* txt_hgt 1.5) p_line_len (* txt_hgt 3.0))
      
      (setq base_data nil)
        (setq v_param 0 end_p (fix (+ (vlax-curve-getEndParam entPline) 0.5)))
        (while (if is_closed_d (< v_param end_p) (<= v_param end_p))
          (setq pt3d (vlax-curve-getPointAtParam entPline v_param))
          
          ;; [수정] 평면도 넘버링 정렬을 위해 각 절점의 누적 거리(Chain) 계산
          (setq cur_dist (vlax-curve-getDistAtParam entPline v_param))
          (if is_closed_d
            (progn
              (if is_forward_d
                (setq chain (- cur_dist base_start_dist_d))
                (setq chain (- base_start_dist_d cur_dist))
              )
              (if (< (abs chain) 0.01) (setq chain 0.0))
              (if (< chain -0.01) (setq chain (+ total_pline_dist chain)))
            )
            (progn
              (setq chain cur_dist)
              (if (> start_pt_dist end_pt_dist) (setq chain (- total_pline_dist chain)))
            )
          )
          
          ;; 해당 절점에 POINT 객체가 존재하는지 검사 (2D 물리적 거리 기준)
          (setq has_point nil)
          (setq v_pt (vlax-curve-getPointAtParam entPline v_param))
          (foreach item result_list
            (setq tmp_pt (caddr item))
            (if (and v_pt (< (distance (list (car tmp_pt) (cadr tmp_pt)) (list (car v_pt) (cadr v_pt))) 1e-3))
              (setq has_point T)
            )
          )
          
          (setq param_prev (max 0.0 (- v_param 1e-5)) param_next (min (vlax-curve-getEndParam entPline) (+ v_param 1e-5)))
          (setq d1 (vlax-curve-getFirstDeriv entPline param_prev) d2 (vlax-curve-getFirstDeriv entPline param_next))
          
          (setq len1 (distance '(0 0) (list (car d1) (cadr d1))))
          (setq len2 (distance '(0 0) (list (car d2) (cadr d2))))
          
          (if (or (equal len1 0.0 1e-8) (equal len2 0.0 1e-8))
            (setq angle_dir (- (angle '(0 0) d1) (/ pi 2.0)))
            (progn
              (setq v_in (list (/ (car d1) len1) (/ (cadr d1) len1)))
              (setq v_out (list (/ (car d2) len2) (/ (cadr d2) len2)))
              (setq v_away1 (list (- (car v_in)) (- (cadr v_in))))
              (setq v_sum (list (+ (car v_away1) (car v_out)) (+ (cadr v_away1) (cadr v_out))))
              (if (equal (distance '(0 0) v_sum) 0.0 1e-6)
                (setq angle_dir (- (angle '(0 0) d1) (/ pi 2.0)))
                (setq angle_dir (angle '(0 0) (list (- (car v_sum)) (- (cadr v_sum)))))
              )
            )
          )
          ;; chain 값을 리스트의 4번째 요소로 추가 저장
          (setq base_data (append base_data (list (list pt3d angle_dir has_point chain))))
          (setq v_param (1+ v_param))
        )

      (defun check-collision (pt1 ang1 pt2 ang2 / c1 c2)
        (setq c1 (polar pt1 ang1 (+ p_line_len p_rad)))
        (setq c2 (polar pt2 ang2 (+ p_line_len p_rad)))
        (< (distance c1 c2) (* p_rad 2.5))
      )

      (setq final_data nil i 0 num_verts (length base_data))
      (setq evade_angles (list (/ pi 4.0) (/ pi 2.0) (/ pi 6.0) (/ pi 3.0) (/ pi 12.0) (* 5.0 (/ pi 12.0))))

      (while (< i num_verts)
        ;; chain 변수까지 꺼내오기
        (setq cur_item (nth i base_data) pt3d (car cur_item) angle_dir (cadr cur_item) has_point (caddr cur_item) chain (cadddr cur_item))
        (setq prev_idx (1- i) next_idx (1+ i) orig_angle_dir angle_dir test_idx 0 collision T)
        
        (while (and collision (< test_idx (length evade_angles)))
          (setq collision nil)
          (if (>= prev_idx 0)
            (if (check-collision pt3d angle_dir (car (nth prev_idx final_data)) (cadr (nth prev_idx final_data)))
              (setq collision T)
            )
          )
          (if (and (not collision) (< next_idx num_verts))
            (if (check-collision pt3d angle_dir (car (nth next_idx base_data)) (cadr (nth next_idx base_data)))
              (setq collision T)
            )
          )
          (if collision
            (progn
              (setq evade_sign (if (= (rem test_idx 2) 0) 1.0 -1.0))
              (setq angle_dir (+ orig_angle_dir (* evade_sign (nth test_idx evade_angles))))
              (setq test_idx (1+ test_idx))
            )
          )
        )
        ;; chain 데이터 포함하여 저장
        (setq final_data (append final_data (list (list pt3d angle_dir has_point chain))))
        (setq i (1+ i))
      )

      (if (= run_mode "P2")
        (create_layer "_바닥번호" 2)
      )

      ;; 거리를 기준으로 정렬하여 1번부터 차례대로 번호가 부여되도록 수정
      (setq final_data_sorted (vl-sort final_data '(lambda (a b) (< (cadddr a) (cadddr b)))))

      (setq i 0 draw_num 1)
      (foreach item final_data_sorted
        (setq pt3d (car item) angle_dir (cadr item) has_point (caddr item))
        
        ;; POINT가 없으면 번호 뒤에 별표(*)를 붙이고 마젠타(6번색)로 표기
        (if has_point
          (setq disp_num (itoa draw_num) txt_col (if (= run_mode "P2") 2 1))
          (setq disp_num (strcat (itoa draw_num) "*") txt_col 6)
        )
        
        (setq line_end (polar pt3d angle_dir p_line_len))
        (setq circle_center (polar line_end angle_dir p_rad))
        (setq text_angle (- angle_dir (/ pi 2.0)))
        
        (if (= run_mode "P2")
          (progn
            (setq hw (* p_rad 0.707106))
            (setq sq_p1 (polar circle_center (+ angle_dir (/ pi 4.0)) p_rad))
            (setq sq_p2 (polar circle_center (+ angle_dir (* 3.0 (/ pi 4.0))) p_rad))
            (setq sq_p3 (polar circle_center (+ angle_dir (* 5.0 (/ pi 4.0))) p_rad))
            (setq sq_p4 (polar circle_center (+ angle_dir (* 7.0 (/ pi 4.0))) p_rad))
            
            (setq line_end_sq (polar circle_center (+ angle_dir pi) hw))
            
            (entmake (list '(0 . "LINE") '(8 . "_바닥번호") (cons 62 txt_col) (cons 10 pt3d) (cons 11 line_end_sq)))
            (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                           '(90 . 4) '(70 . 1) '(8 . "_바닥번호") (cons 62 txt_col)
                           (cons 10 sq_p1) (cons 10 sq_p2) (cons 10 sq_p3) (cons 10 sq_p4)))
            (entmake (list '(0 . "TEXT") '(8 . "_바닥번호") (cons 62 txt_col) (cons 10 circle_center) (cons 11 circle_center)
                           (cons 40 txt_hgt) (cons 50 text_angle) (cons 1 disp_num) 
                           (cons 7 txt_sty) '(72 . 1) '(73 . 2)))
          )
          (progn
            (entmake (list '(0 . "LINE") '(8 . "_joint") (cons 62 txt_col) (cons 10 pt3d) (cons 11 line_end)))
            (entmake (list '(0 . "CIRCLE") '(8 . "_joint") (cons 62 txt_col) (cons 10 circle_center) (cons 40 p_rad)))
            (entmake (list '(0 . "TEXT") '(8 . "_joint") (cons 62 txt_col) (cons 10 circle_center) (cons 11 circle_center)
                           (cons 40 txt_hgt) (cons 50 text_angle) (cons 1 disp_num) 
                           (cons 7 txt_sty) '(72 . 1) '(73 . 2)))
          )
        )
        (setq draw_num (1+ draw_num))
      )

      ;;;;==========================================================================
      ;;;; 최대깊이산정 축척 역산 로직
      ;;;;==========================================================================
      (initget "1:1 1:1000 S")
      (setq kw (getkword "\n적용할 축척 입력 : [1:1 / 1:1000 / 직접입력(S)] <1:1000>: "))
      (cond
        ((or (null kw) (= kw "1:1000")) (setq scl 1.0))
        ((= kw "1:1") (setq scl 1000.0))
        ((= kw "S") 
         (setq custom_scl (getreal "\n직접입력할 축척 분모값을 입력하세요 (예: 1:20 이면 20 입력): ")) 
         (if custom_scl 
           (setq scl (/ 1000.0 custom_scl)) 
           (setq scl 1.0)
         )
        )
      )

      (setq insPt (getpoint "\n종단면도 생성 기준점 클릭(가로바닥선 맨 왼쪽 끝점이 됩니다): "))
          
          (if insPt
            (progn
              (setq old_osmode (getvar "OSMODE"))
              (setvar "OSMODE" 0)

              (setq base_chain 0.0)
              (setq profile_len total_pline_dist)

          (create_layer "_HRZ" 7)
          (create_layer "_GRND" 6)
          (create_layer "_굴착바닥고" 4)
          (create_layer "_TXT" 7)

          (setq horiz_start insPt horiz_end (list (+ (car insPt) profile_len) (cadr insPt)))
          (entmake (list '(0 . "LINE") (cons 10 horiz_start) (cons 11 horiz_end) '(8 . "_HRZ") '(62 . 256)))

          ;; T, C 모드 또는 P1(지표면 종단) 모드인 경우
          (if (or (= run_mode "T") (= run_mode "C") (= run_mode "P1"))
            (progn
              (create_layer "_LEVL" 7)
              (create_layer "_VERT" 8)
              
              ;; T 또는 C 모드일 때 평면상에 임의의 LEVL 포인트/텍스트 추가 생성
              (if (or (= run_mode "T") (= run_mode "C"))
                (foreach item result_list
                  (setq pt3d (list (car (caddr item)) (cadr (caddr item)) (cadr item)))
                  (entmake (list '(0 . "POINT") (cons 10 pt3d) '(8 . "_LEVL") '(62 . 256)))
                  (entmake (list '(0 . "TEXT") (cons 10 pt3d) (cons 40 txt_hgt) (cons 1 (rtos (cadr item) 2 2)) (cons 7 txt_sty) '(8 . "_LEVL") '(62 . 256)))
                )
              )

              (setq ent_list (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 90 (length result_list)) '(70 . 0) '(8 . "_GRND") '(62 . 256)))
              (foreach item result_list
                (setq cad_x (+ (car insPt) (- (car item) base_chain)) cad_y (+ (cadr insPt) (* (cadr item) scl)))
                (setq ent_list (append ent_list (list (cons 10 (list cad_x cad_y)))))
              )
              (entmake ent_list)

              (foreach item result_list
                (setq cad_x (+ (car insPt) (- (car item) base_chain)) cad_y (+ (cadr insPt) (* (cadr item) scl)))
                (entmake (list '(0 . "LINE") (cons 10 (list cad_x (cadr insPt))) (cons 11 (list cad_x cad_y)) '(8 . "_VERT") '(62 . 8)))
                (setq txt_pt (list cad_x (- (cadr insPt) txt_hgt)))
                (entmake (list '(0 . "TEXT") (cons 10 txt_pt) (cons 11 txt_pt) (cons 40 txt_hgt) (cons 1 (rtos (cadr item) 2 2)) 
                               (cons 7 txt_sty) (cons 50 (/ pi 2.0)) '(72 . 2) '(73 . 2) '(8 . "_TXT") '(62 . 256)))
              )
            )
          )

                    ;; P2(굴착바닥고) 모드인 경우
          (if (= run_mode "P2")
            (progn
              (setq valid_segs nil i 0)
              (while (< i (1- (length result_list)))
                (setq p_cur (nth i result_list) p_next (nth (1+ i) result_list))
                (setq x_start (- (car p_cur) base_chain) x_end (- (car p_next) base_chain) y_cur (cadr p_cur))
                (if (> (abs (- x_end x_start)) 1e-3) (setq valid_segs (append valid_segs (list (list x_start x_end y_cur)))))
                (setq i (1+ i))
              )
              ;; 열린 선형일 때 마지막으로 찍힌 포인트의 Z값이 선형의 맨 끝점(total_pline_dist)까지 자동으로 수평 연장되도록 처리
              (if (and (not is_closed_d) (> (length result_list) 0))
                (progn
                  (setq last_pt_item (last result_list))
                  (setq last_x (- (car last_pt_item) base_chain))
                  (setq last_y (cadr last_pt_item))
                  (if (> (- profile_len last_x) 1e-3)
                    (setq valid_segs (append valid_segs (list (list last_x profile_len last_y))))
                  )
                )
              )

              (if (null valid_segs) (progn (alert "유효한 수평 구간이 없습니다.") (restore_sys) (exit)))

              (setq final_segs nil cliff_jumps nil j 0)
              (while (< j (length valid_segs))
                (setq cur_seg (nth j valid_segs) x_start (car cur_seg) x_end (cadr cur_seg) y_cur (caddr cur_seg))
                ;; 진행방향 기준 일괄 앞구간 차감: 뒷구간(현재 구간)의 시작점은 항상 정위치(x_start)를 그대로 유지
                (setq final_start_x x_start final_end_x x_end modified_start nil modified_end nil)

                (if (< j (1- (length valid_segs)))
                  (progn
                    (setq next_seg (nth (1+ j) valid_segs) y_next (caddr next_seg))
                    ;; 오르막/내리막과 무관하게 단차가 발생하면 무조건 앞구간(현재 구간)의 끝점(x_end)에서 dx만큼 차감
                    (if (> (abs (- y_cur y_next)) 1e-3)
                      (progn
                        (setq dx (* (abs (- y_cur y_next)) gubae scl))
                        (if (>= (- x_end dx) final_start_x)
                          (progn (setq final_end_x (- x_end dx)) (setq modified_end T))
                          (setq cliff_jumps (append cliff_jumps (list (list x_end y_next (if (> y_cur y_next) "DOWN" "UP")))))
                        )
                      )
                    )
                  )
                  ;; 폐합선 마감 (마지막 세그먼트 -> 첫 세그먼트 단차를 끝에서 깎아줌)
                  (if is_closed_d
                    (progn
                      (setq first_seg (nth 0 valid_segs) y_first (caddr first_seg))
                      (if (> (abs (- y_cur y_first)) 1e-3)
                        (progn
                          (setq dx (* (abs (- y_cur y_first)) gubae scl))
                          (if (>= (- x_end dx) final_start_x)
                            (progn (setq final_end_x (- x_end dx)) (setq modified_end T))
                            (setq cliff_jumps (append cliff_jumps (list (list x_end y_first (if (> y_cur y_first) "DOWN" "UP")))))
                          )
                        )
                      )
                    )
                  )
                )

                (if (> final_start_x final_end_x)
                  (progn
                    (setq final_start_x x_start final_end_x x_end)
                    (if modified_end 
                      (if (and is_closed_d (= j (1- (length valid_segs))))
                        (setq cliff_jumps (append cliff_jumps (list (list x_end y_first (if (> y_cur y_first) "DOWN" "UP")))))
                        (setq cliff_jumps (append cliff_jumps (list (list x_end y_next (if (> y_cur y_next) "DOWN" "UP")))))
                      )
                    )
                  )
                )

                (setq final_segs (append final_segs (list (list final_start_x final_end_x y_cur))))
                (setq j (1+ j))
              )

              (setq vert_count (* (length final_segs) 2))
              (if is_closed_d (setq vert_count (1+ vert_count)))

              (setq ent_list (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                                   (cons 90 vert_count) '(70 . 0) '(8 . "_굴착바닥고") '(62 . 256)))
              
              (foreach seg final_segs
                (setq cad_x1 (+ (car insPt) (car seg)) cad_x2 (+ (car insPt) (cadr seg)) cad_y (+ (cadr insPt) (* (caddr seg) scl)))
                (setq ent_list (append ent_list (list (cons 10 (list cad_x1 cad_y)))))
                (setq ent_list (append ent_list (list (cons 10 (list cad_x2 cad_y)))))

                (if (> (abs (- cad_x2 cad_x1)) 1e-4)
                  (progn
                    (setq center_x (/ (+ cad_x1 cad_x2) 2.0) txt_pt (list center_x (- cad_y (* 0.5 txt_hgt))))
                    (entmake (list '(0 . "TEXT") (cons 10 txt_pt) (cons 11 txt_pt) (cons 40 txt_hgt)
                                   (cons 1 (rtos (caddr seg) 2 2)) (cons 7 txt_sty) (cons 50 0.0)
                                   '(72 . 1) '(73 . 3) '(8 . "_TXT") '(62 . 256)))
                  )
                )
              )
              
              (if is_closed_d
                (progn
                  (setq first_seg (nth 0 valid_segs))
                  (setq close_x (+ (car insPt) total_pline_dist))
                  (setq close_y (+ (cadr insPt) (* (caddr first_seg) scl)))
                  (setq ent_list (append ent_list (list (cons 10 (list close_x close_y)))))
                )
              )
              (setq exc_poly_ent (entmakex ent_list))

              (foreach cj cliff_jumps
                (setq bx (+ (car insPt) (car cj)) by (+ (cadr insPt) (* (cadr cj) scl)) dir (caddr cj))
                (if (= dir "UP") (setq vx gubae vy 1.0) (setq vx (- gubae) vy 1.0))
                (setq v_len (sqrt (+ (* vx vx) (* vy vy))) u_x (/ vx v_len) u_y (/ vy v_len) draw_len 1000.0)
                (setq pt_start (list bx by) pt_end (list (+ bx (* u_x draw_len)) (+ by (* u_y draw_len))))
                (entmake (list '(0 . "LINE") (cons 10 pt_start) (cons 11 pt_end) '(8 . "_굴착바닥고") '(62 . 256)))
              )

              ;; P2(굴착바닥고) 모드 수직선(_VERT2) 생성
              (create_layer "_VERT2" 9)
              (foreach item result_list
                (setq cad_x (+ (car insPt) (- (car item) base_chain)) cad_y (+ (cadr insPt) (* (cadr item) scl)))
                (entmake (list '(0 . "LINE") (cons 10 (list cad_x (cadr insPt))) (cons 11 (list cad_x cad_y)) '(8 . "_VERT2") '(62 . 9)))
              )
            )
          )

          ;; =====================================================================
          ;; 종단면도 상단 일괄 넘버링 기입
          ;; =====================================================================
          ;; P2(굴착바닥고) 모드일 경우 _바닥번호 도면층 생성
          (if (= run_mode "P2")
            (create_layer "_바닥번호" 2)
          )

          (setq max_vert_hgt 0.0)
          (foreach item result_list
            (setq tmp_h (* (cadr item) scl))
            (if (> tmp_h max_vert_hgt) (setq max_vert_hgt tmp_h))
          )
          
          (setq leader_bottom_y (+ (cadr insPt) max_vert_hgt (* 3.0 scl)))
          (setq leader_top_y (+ leader_bottom_y (* 3.0 scl)))
          (setq circle_y (+ leader_top_y p_rad))

          ;; 정렬을 위한 리스트 선언
          (setq profile_verts nil)
          (setq v_param 0 end_p (fix (+ (vlax-curve-getEndParam entPline) 0.5)))
          (while (if is_closed_d (< v_param end_p) (<= v_param end_p))
            ;; 현재 절점의 누적 거리 계산
            (setq cur_dist (vlax-curve-getDistAtParam entPline v_param))
            (if is_closed_d
              (progn
                (if is_forward_d
                  (setq chain (- cur_dist base_start_dist_d))
                  (setq chain (- base_start_dist_d cur_dist))
                )
                (if (< (abs chain) 0.01) (setq chain 0.0))
                (if (< chain -0.01) (setq chain (+ total_pline_dist chain)))
              )
              (progn
                (setq chain cur_dist)
                (if (> start_pt_dist end_pt_dist) (setq chain (- total_pline_dist chain)))
              )
            )
            
            ;; 현재 절점에 POINT 객체가 존재하는지 검사 (2D 물리적 거리 기준)
            (setq has_point nil)
            (setq v_pt (vlax-curve-getPointAtParam entPline v_param))
            (foreach item result_list
              (setq tmp_pt (caddr item))
              (if (and v_pt (< (distance (list (car tmp_pt) (cadr tmp_pt)) (list (car v_pt) (cadr v_pt))) 1e-3))
                (setq has_point T)
              )
            )
            
            (setq profile_verts (append profile_verts (list (list chain has_point))))
            (setq v_param (1+ v_param))
          )

          ;; ★ 핵심: 수집된 절점들을 진행 거리(chain)순으로 정렬
          (setq profile_verts (vl-sort profile_verts '(lambda (a b) (< (car a) (car b)))))
          
          ;; 폐합선인 경우 마지막에 0m(시점) 위치를 전체 길이 위치에 하나 더 추가 복사
          (if is_closed_d
            (progn
              (setq first_vert (nth 0 profile_verts))
              (setq profile_verts (append profile_verts (list (list total_pline_dist (cadr first_vert)))))
            )
          )

          (setq i 0 total_res (length profile_verts))
          (foreach vert profile_verts
            (setq chain (car vert) has_point (cadr vert))
            (setq cad_x (+ (car insPt) chain))
            (setq p_line_start (list cad_x leader_bottom_y)) 
            (setq p_line_end (list cad_x leader_top_y)) 
            (setq num_pt (list cad_x circle_y))
            
            ;; 폐합선 마지막 점이면 번호를 "1"로 강제 지정, 아니면 순차적 번호
            (if (and is_closed_d (= i (1- total_res)))
              (setq disp_num "1")
              (setq disp_num (itoa (1+ i)))
            )
            
            ;; POINT 누락 시 마젠타(6번색) 및 별표(*) 표시
            (if has_point
              (setq txt_col (if (= run_mode "P2") 2 1))
              (setq disp_num (strcat disp_num "*") txt_col 6)
            )

            (if (= run_mode "P2")
              (progn
                (setq hw (* p_rad 0.707106))
                (setq sq_pt1 (list (- cad_x hw) (- circle_y hw)))
                (setq sq_pt2 (list (+ cad_x hw) (- circle_y hw)))
                (setq sq_pt3 (list (+ cad_x hw) (+ circle_y hw)))
                (setq sq_pt4 (list (- cad_x hw) (+ circle_y hw)))
                (setq p_line_end_sq (list cad_x (- circle_y hw)))
                
                (entmake (list '(0 . "LINE") '(8 . "_바닥번호") (cons 62 txt_col) (cons 10 p_line_start) (cons 11 p_line_end_sq)))
                (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                               '(90 . 4) '(70 . 1) '(8 . "_바닥번호") (cons 62 txt_col)
                               (cons 10 sq_pt1) (cons 10 sq_pt2) (cons 10 sq_pt3) (cons 10 sq_pt4)))
                (entmake (list '(0 . "TEXT") '(8 . "_바닥번호") (cons 62 txt_col) (cons 10 num_pt) (cons 11 num_pt)
                               (cons 40 txt_hgt) (cons 50 0.0) (cons 1 disp_num) 
                               (cons 7 txt_sty) '(72 . 1) '(73 . 2)))
              )
              (progn
                (entmake (list '(0 . "LINE") '(8 . "_joint") (cons 62 txt_col) (cons 10 p_line_start) (cons 11 p_line_end)))
                (entmake (list '(0 . "CIRCLE") '(8 . "_joint") (cons 62 txt_col) (cons 10 num_pt) (cons 40 p_rad)))
                (entmake (list '(0 . "TEXT") '(8 . "_joint") (cons 62 txt_col) (cons 10 num_pt) (cons 11 num_pt)
                               (cons 40 txt_hgt) (cons 50 0.0) (cons 1 disp_num) 
                               (cons 7 txt_sty) '(72 . 1) '(73 . 2)))
              )
            )
            (setq i (1+ i))
          )

          ;; =====================================================================
          ;; 최대 깊이 계산 및 표기 분기 (P2 모드 전용)
          ;; =====================================================================
          (if (and (= run_mode "P2") exc_poly_ent)
            (progn
              (initget "Y N")
              (setq calc_max (getkword "\n최대 깊이를 계산하시겠습니까? [계산(Y) / 종료(N)] <Y>: "))
              (if (or (null calc_max) (= calc_max "Y"))
                (progn
                  (setq grnd_sel (entsel "\n비교할 지표면(_GRND) 선형을 선택하세요: "))
                  (if grnd_sel
                    (progn
                      (setq grnd_ent (car grnd_sel))

                      ;; 1. 두 선의 모든 X 좌표를 추출하여 병합 및 정렬
                      (setq x_list (append (get-pline-vertices-x exc_poly_ent) (get-pline-vertices-x grnd_ent)))
                      (setq x_list (vl-sort x_list '<))
                      
                      ;; 2. 중복 X 좌표 제거 (캐드 부동소수점 오차 허용)
                      (setq clean_x nil prev_x -1e9)
                      (foreach x x_list
                        (if (> (abs (- x prev_x)) 1e-3)
                          (setq clean_x (append clean_x (list x)) prev_x x)
                        )
                      )
                      
                      ;; 3. 각 X 좌표에서 수직 거리를 계산하여 최대값 도출
                      (setq max_dist -1.0 max_x nil max_y1 nil max_y2 nil)
                      (foreach x clean_x
                        (setq y1 (get-y-on-pline exc_poly_ent x))
                        (setq y2 (get-y-on-pline grnd_ent x))
                        (if (and y1 y2)
                          (progn
                            (setq dist (abs (- y1 y2)))
                            (if (> dist max_dist)
                              (setq max_dist dist max_x x max_y1 y1 max_y2 y2)
                            )
                          )
                        )
                      )
                      
                      ;; 4. 결과 출력 (세로선 및 텍스트 기입)
                      (if (> max_dist 0)
                        (progn
                          ;; 바닥굴착고 텍스트와 동일한 _TXT 도면층 및 색상(256) 사용
                          (entmake (list '(0 . "LINE") (cons 10 (list max_x max_y1)) (cons 11 (list max_x max_y2)) '(8 . "_TXT") '(62 . 256)))
                          
                          (setq mid_y (/ (+ max_y1 max_y2) 2.0))
                          (setq real_dist (/ max_dist scl))
                          
                          ;; MAX 문자 기입 (세로선 왼쪽, 90도 회전, 중앙 정렬)
                          (setq max_txt_pt (list (- max_x (* txt_hgt 0.7)) mid_y))
                          (entmake (list '(0 . "TEXT") (cons 10 max_txt_pt) (cons 11 max_txt_pt) (cons 40 txt_hgt)
                                         (cons 1 "MAX") (cons 7 txt_sty) (cons 50 (/ pi 2.0))
                                         '(72 . 1) '(73 . 2) '(8 . "_TXT") '(62 . 256)))

                          ;; 깊이 수치 기입 (세로선 오른쪽, 90도 회전, 중앙 정렬)
                          (setq dist_txt_pt (list (+ max_x (* txt_hgt 0.7)) mid_y))
                          (entmake (list '(0 . "TEXT") (cons 10 dist_txt_pt) (cons 11 dist_txt_pt) (cons 40 txt_hgt)
                                         (cons 1 (rtos real_dist 2 2)) (cons 7 txt_sty) (cons 50 (/ pi 2.0))
                                         '(72 . 1) '(73 . 2) '(8 . "_TXT") '(62 . 256)))
                          
                          (princ (strcat "\n-> 가장 깊은 위치에 세로선을 긋고 최대 깊이(" (rtos real_dist 2 2) ")를 표기했습니다."))
                        )
                        (princ "\n-> 두 선이 수직으로 겹치는 구간을 찾을 수 없습니다.")
                      )
                    )
                    (princ "\n-> 선택이 취소되었습니다.")
                  )
                )
              )
            )
          )

          (setvar "OSMODE" old_osmode)
              (princ "\n작업이 완료되었습니다.")
            ) 
          )  
        )    
      )       
    )         
  )           

  ((= main_opt "M") 
    (progn ; [최대깊이산정(M) 모드 시작]
      ;; 1. 스타일 및 스케일 설정 (LData를 활용한 속성 저장 및 불러오기)
      (setq txt_hgt nil txt_sty nil loop_txt T)
      (while loop_txt
        (setq txt_sel (entsel "\n표기에 사용할 텍스트 높이와 스타일을 가져올 문자(TEXT/MTEXT)를 선택하세요 (엔터: 도면 저장값 사용): "))
        (if txt_sel
          (progn
            (setq txt_ent (car txt_sel) txt_data (entget txt_ent) obj_type (cdr (assoc 0 txt_data)))
            (if (member obj_type '("TEXT" "MTEXT"))
              (progn
                (setq txt_sty (cdr (assoc 7 txt_data)) txt_hgt (cdr (assoc 40 txt_data)))
                (vlax-ldata-put "DRG_SETTINGS" "TXT_HGT" txt_hgt)
                (vlax-ldata-put "DRG_SETTINGS" "TXT_STY" txt_sty)
                (princ (strcat "\n-> 선택한 객체의 속성을 적용 및 저장했습니다. (높이: " (rtos txt_hgt 2 2) ", 스타일: " txt_sty ")"))
                (setq loop_txt nil)
              )
              (princ (strcat "\n-> 선택한 객체는 " obj_type " 입니다. 문자 객체를 다시 클릭해주세요."))
            )
          )
          (progn
            (setq saved_hgt (vlax-ldata-get "DRG_SETTINGS" "TXT_HGT"))
            (setq saved_sty (vlax-ldata-get "DRG_SETTINGS" "TXT_STY"))
            (if (and saved_hgt saved_sty)
              (progn
                (setq txt_hgt saved_hgt txt_sty saved_sty)
                (princ (strcat "\n-> 도면에 저장된 이전 속성을 불러옵니다. (높이: " (rtos txt_hgt 2 2) ", 스타일: " txt_sty ")"))
                (setq loop_txt nil)
              )
              (princ "\n저장된 텍스트 속성이 없습니다. 텍스트 객체를 선택해주세요.")
            )
          )
        )
      )

      ;;;;==========================================================================
      ;;;; 종단 축척 입력 및 변환 로직 (수직 과장 적용)
      ;;;;==========================================================================
      (initget "1:1 1:1000 S")
      (setq kw (getkword "\n적용할 축척 입력 : [1:1 / 1:1000 / 직접입력(S)] <1:1>: "))
      (cond
        ((or (null kw) (= kw "1:1")) (setq scl 1000.0))
        ((= kw "1:1000") (setq scl 1.0))
        ((= kw "S") 
         (setq custom_scl (getreal "\n직접입력할 축척 분모값을 입력하세요 (예: 1:20 이면 20 입력): ")) 
         (if custom_scl 
           (setq scl (/ 1000.0 custom_scl)) 
           (setq scl 1.0)
         )
        )
      )

      ;; 2. 객체 선택 (드래그 선택 및 개수 검증 루프)
      (setq loop_sel T)
      (while loop_sel
        (princ "\n최대 깊이를 산정할 두 선형을 드래그하여 선택하세요 (정확히 2개): ")
        (setq ss (ssget '((0 . "LWPOLYLINE,LINE"))))
        (if (null ss)
          (progn (princ "\n선택된 객체가 없습니다.") (setq loop_sel nil))
          (if (/= (sslength ss) 2)
            (princ (strcat "\n오류: 선택된 선형이 " (itoa (sslength ss)) "개입니다. 2개보다 많거나 적습니다. 다시 선택해주세요."))
            (progn
              (setq ent1 (ssname ss 0) ent2 (ssname ss 1))
              (setq loop_sel nil)
            )
          )
        )
      )

      (if (and ent1 ent2)
        (progn

          ;; 3. 계산 및 출력
          (setq x_list (vl-sort (append (get-pline-vertices-x ent1) (get-pline-vertices-x ent2)) '<))
          (setq clean_x nil prev_x -1e9)
          (foreach x x_list (if (> (abs (- x prev_x)) 1e-3) (setq clean_x (append clean_x (list x)) prev_x x)))
          
          (setq max_dist -1.0 max_x nil max_y1 nil max_y2 nil)
          (foreach x clean_x
            (setq y1 (get-y-on-pline ent1 x) y2 (get-y-on-pline ent2 x))
            (if (and y1 y2)
              (progn
                (setq dist (abs (- y1 y2)))
                (if (> dist max_dist) (setq max_dist dist max_x x max_y1 y1 max_y2 y2))
              )
            )
          )
          
          (if (> max_dist 0)
            (progn
              (create_layer "_TXT" 7)
              ;; 세로선 그리기 (Z좌표 0.0 명시)
              (entmake (list '(0 . "LINE") (cons 10 (list max_x max_y1 0.0)) (cons 11 (list max_x max_y2 0.0)) '(8 . "_TXT") '(62 . 256)))
              
              ;; 공통 중간 높이 계산
              (setq mid_y (/ (+ max_y1 max_y2) 2.0))
              
              ;;;;==========================================================================
              ;;;; 1. MAX 문자 기입 (세로선 왼쪽, 90도 회전, 중앙 정렬, 3D 좌표 적용)
              ;;;;==========================================================================
              (setq max_txt_pt (list (- max_x (* txt_hgt 0.7)) mid_y 0.0))
              (entmake (list '(0 . "TEXT") 
                             (cons 10 max_txt_pt) 
                             (cons 11 max_txt_pt) 
                             (cons 40 txt_hgt)
                             (cons 1 "MAX") 
                             (cons 7 txt_sty) 
                             (cons 50 (/ pi 2.0)) ;; 90도 회전
                             '(72 . 1) '(73 . 2)  ;; 중간 중심 정렬
                             '(8 . "_TXT") 
                             '(62 . 256)))

              ;;;;==========================================================================
              ;;;; 2. 깊이 수치 기입 (세로선 오른쪽, 90도 회전, 중앙 정렬, 3D 좌표 적용)
              ;;;;==========================================================================
              (setq dist_txt_pt (list (+ max_x (* txt_hgt 0.7)) mid_y 0.0))
              (entmake (list '(0 . "TEXT") 
                             (cons 10 dist_txt_pt) 
                             (cons 11 dist_txt_pt) 
                             (cons 40 txt_hgt)
                             (cons 1 (rtos (/ max_dist scl) 2 2)) 
                             (cons 7 txt_sty) 
                             (cons 50 (/ pi 2.0)) ;; 90도 회전
                             '(72 . 1) '(73 . 2)  ;; 중간 중심 정렬
                             '(8 . "_TXT") 
                             '(62 . 256)))

              (princ (strcat "\n-> 최대 깊이 " (rtos (/ max_dist scl) 2 2) " 산정 완료."))
            )
            (princ "\n-> 겹치는 구간이 없습니다.")
          )
        )
      )
    )
    ) ; [최대깊이산정(M) 모드 끝]

    ((= main_opt "E")
    (progn ; [전개도작성(E) 모드 시작]
      (initget "T M")
      (setq e_src_opt (getkword "\n전개도 작도 방식 선택 [TSP 프로젝트 연동(T) / 기존 수동입력(M)] <T>: "))
      (if (null e_src_opt) (setq e_src_opt "T"))
      (if (= e_src_opt "T")
        (progn
          (drg-run-elevation-with-tsp nil nil nil)
          (restore_sys)
          (exit)
        )
      )
      ;;;;==========================================================================
      ;;;; defun get-num-from-circle : 원의 중심점과 가장 가까운 텍스트의 숫자를 추출
      ;;;; (원이 겹쳐있을 때, 단순 교차선택이 아닌 거리 계산을 통해 정확한 텍스트 매칭)
      ;;;;==========================================================================
      (defun get-num-from-circle (c_ent / c_data c_center c_rad ss_txt k t_ent t_data pt10 pt11 t_pt dist min_dist best_num)
        (setq c_data (entget c_ent) c_center (cdr (assoc 10 c_data)) c_rad (cdr (assoc 40 c_data)))
        (setq ss_txt (ssget "C" 
                       (list (- (car c_center) c_rad) (- (cadr c_center) c_rad)) 
                       (list (+ (car c_center) c_rad) (+ (cadr c_center) c_rad)) 
                       '((0 . "TEXT,MTEXT"))))
        (setq best_num 0 min_dist 1e9)
        (if ss_txt
          (progn
            (setq k 0)
            (while (< k (sslength ss_txt))
              (setq t_ent (ssname ss_txt k) t_data (entget t_ent))
              (setq pt10 (cdr (assoc 10 t_data)) pt11 (cdr (assoc 11 t_data)))
              ;; 문자의 정렬 방식에 따라 기준점(좌측정렬은 10번, 그 외는 11번) 결정
              (if (and (equal (cdr (assoc 72 t_data)) 0) (equal (cdr (assoc 73 t_data)) 0))
                (setq t_pt pt10)
                (setq t_pt pt11)
              )
              ;; Z값을 무시하고 XY 평면 상의 거리만 계산하여 최단 거리 탐색
              (setq dist (distance (list (car c_center) (cadr c_center)) (list (car t_pt) (cadr t_pt))))
              (if (< dist min_dist)
                (progn
                  (setq min_dist dist)
                  (setq best_num (atoi (cdr (assoc 1 t_data))))
                )
              )
              (setq k (1+ k))
            )
          )
        )
        best_num
      )

      ;;;;==========================================================================
      ;;;; 1. 기준선 선택 및 중복선(겹친 _HRZ 등) 자동 삭제 로직
      ;;;;==========================================================================
      (princ "\n전개도 작성을 위해 기준선 3개(_GRND, _굴착바닥고, _HRZ)를 선택하세요: ")
      (setq ss_lines (ssget '((0 . "LINE,LWPOLYLINE"))))
      (if (null ss_lines)
        (progn (alert "선택된 객체가 없습니다.") (restore_sys) (exit))
      )
      
      (setq raw_list nil i 0)
      (while (< i (sslength ss_lines))
        (setq raw_list (append raw_list (list (ssname ss_lines i))))
        (setq i (1+ i))
      )
      
      ;; 완벽히 겹치는 선형(시작점과 끝점이 동일한 경우) 검사 및 삭제
      (setq unique_list nil deleted_count 0)
      (foreach ent raw_list
        (setq p_start (vlax-curve-getStartPoint ent) p_end (vlax-curve-getEndPoint ent))
        (setq is_dup nil)
        (foreach u_ent unique_list
          (setq u_start (vlax-curve-getStartPoint u_ent) u_end (vlax-curve-getEndPoint u_ent))
          ;; 방향과 무관하게 양 끝점의 위치가 오차 범위(1e-3) 내에서 일치하는지 확인
          (if (or (and (< (distance p_start u_start) 1e-3) (< (distance p_end u_end) 1e-3))
                  (and (< (distance p_start u_end) 1e-3) (< (distance p_end u_start) 1e-3)))
            (setq is_dup T)
          )
        )
        (if is_dup
          (progn
            (entdel ent) ;; 도면에서 중복 객체를 완전히 삭제
            (setq deleted_count (1+ deleted_count))
          )
          (setq unique_list (append unique_list (list ent)))
        )
      )

      (if (> deleted_count 0)
        (princ (strcat "\n-> 완벽히 겹치는 중복 선형 " (itoa deleted_count) "개를 도면에서 삭제했습니다."))
      )

      ;; 중복을 제거한 후 순수하게 남은 선이 3개인지 검증
      (if (/= (length unique_list) 3)
        (progn 
          (alert (strcat "기준선은 중복 제외 정확히 3개여야 합니다. (현재 유효한 선: " (itoa (length unique_list)) "개)")) 
          (restore_sys) 
          (exit)
        )
      )
      
      ;; 3개의 선을 Y좌표(높이) 기준으로 분류 (_GRND, _굴착바닥고, _HRZ)
      (setq line_list nil)
      (foreach ent unique_list
        (setq p_start (vlax-curve-getStartPoint ent) p_end (vlax-curve-getEndPoint ent))
        (setq avg_y (/ (+ (cadr p_start) (cadr p_end)) 2.0))
        (setq line_list (append line_list (list (list avg_y ent))))
      )
      (setq line_list (vl-sort line_list '(lambda (a b) (> (car a) (car b)))))
      (setq ent_grnd (cadr (nth 0 line_list)))
      (setq ent_excav (cadr (nth 1 line_list)))
      (setq ent_hrz (cadr (nth 2 line_list)))
      
      ;;;;==========================================================================
      ;;;; 2. 넘버링 검증 및 거리 확인
      ;;;;==========================================================================
      (princ "\n전체 번호(원)를 드래그하여 선택하세요: ")
      (setq ss_circles (ssget '((0 . "CIRCLE") (8 . "_joint"))))
      (if (null ss_circles) (progn (alert "원이 선택되지 않았습니다.") (restore_sys) (exit)))
      
      (setq circle_list nil i 0)
      (while (< i (sslength ss_circles))
        (setq c_ent (ssname ss_circles i) c_data (entget c_ent))
        (setq c_center (cdr (assoc 10 c_data)) c_rad (cdr (assoc 40 c_data)))
        (setq num (get-num-from-circle c_ent))
        (if (> num 0) 
          (setq circle_list (append circle_list (list (list num (car c_center) (cadr c_center) c_rad))))
        )
        (setq i (1+ i))
      )
      (setq circle_list (vl-sort circle_list '(lambda (a b) (< (cadr a) (cadr b)))))
      
      (setq is_valid_nums T item_idx 0 total_c (length circle_list))
      (if (> total_c 0) (setq prev_num (1- (car (nth 0 circle_list)))))
      
      (foreach item circle_list
        (setq cur_num (car item))
        (if (/= cur_num (1+ prev_num))
          ;; 마지막 원이 폐합선으로 인해 '1'이 된 경우는 정상적인 시퀀스로 허용
          (if (not (and (= item_idx (1- total_c)) (= cur_num 1)))
            (setq is_valid_nums nil)
          )
        )
        (setq prev_num cur_num)
        (setq item_idx (1+ item_idx))
      )
      (if (not is_valid_nums) (progn (alert "번호가 순차적이지 않거나 누락된 번호가 있습니다.") (restore_sys) (exit)))
      
      (setq hrz_p1 (vlax-curve-getStartPoint ent_hrz) hrz_p2 (vlax-curve-getEndPoint ent_hrz))
      (setq hrz_len (abs (- (car hrz_p2) (car hrz_p1))))
      (setq circle_dist (- (cadr (last circle_list)) (cadr (nth 0 circle_list))))
      (if (> (abs (- hrz_len circle_dist)) 1.0)
        (progn (alert "가로선(_HRZ)의 길이와 원 전체 간의 거리가 일치하지 않습니다. (오차 1.0 초과)") (restore_sys) (exit))
      )
      
      ;;;;==========================================================================
      ;;;; 3. 측면말뚝 배치 방식 선택 및 제원 입력
      ;;;;==========================================================================
      (setq total_circles (length circle_list))
      (setq prev_section_max_x (cadr (nth 0 circle_list)))
      (setq final_target_x (cadr (last circle_list)))
      (setq current_target_num (car (nth 0 circle_list)))
      (setq all_piles nil) 
      (setq last_pile_x prev_section_max_x)
      (setq is_loop_done nil)

      (princ "\n평면도의 측면말뚝 단면(블록) 및 지시 객체를 드래그하여 선택하세요 (엔터: 기존 C.T.C 직접 입력): ")
      (setq ss_plan (ssget '((8 . "_지시,_측면말뚝"))))

      (if ss_plan
        (progn
          ;;;;--------------------------------------------------------------------------
          ;;;; [방식 A] 평면도 객체 투영 매칭 및 지시 객체 재생성
          ;;;;--------------------------------------------------------------------------
          (setq ent_bound_sel nil)
          (while (null ent_bound_sel)
            (setq ent_bound_sel (entsel "\n평면도 경계선을 선택하세요 (클릭 지점이 전개도 시점이 됩니다): "))
            (if (null ent_bound_sel) (princ "\n경계선이 선택되지 않았습니다. 다시 선택해주세요."))
          )
          (setq ent_bound (car ent_bound_sel) bound_pick_pt (cadr ent_bound_sel))
          
          (setq total_bound_dist (vlax-curve-getDistAtParam ent_bound (vlax-curve-getEndParam ent_bound)))
          (setq prof_start_x (min (car hrz_p1) (car hrz_p2)))
          
          (setq is_closed (vlax-curve-isClosed ent_bound))
          (if is_closed
            (progn
              ;; 폐합된 도형일 경우 사용자에게 명시적 시점과 방향을 요구
              (setq manual_start_pt (getpoint "\n폐합된 선형입니다. 전개도의 0m 시점으로 사용할 정확한 위치(끝점 등)를 클릭하세요: "))
              (setq manual_dir_pt (getpoint manual_start_pt "\n전개도 진행 방향에 있는 선형 위의 임의의 점을 클릭하세요: "))
              
              (setq base_start_param (vlax-curve-getParamAtPoint ent_bound (vlax-curve-getClosestPointTo ent_bound manual_start_pt)))
              (setq base_start_dist (vlax-curve-getDistAtParam ent_bound base_start_param))
              
              (setq dir_param (vlax-curve-getParamAtPoint ent_bound (vlax-curve-getClosestPointTo ent_bound manual_dir_pt)))
              (setq dir_dist (vlax-curve-getDistAtParam ent_bound dir_param))
              
              ;; 방향 판별: 선형의 고유 진행 방향이 사용자가 찍은 방향과 일치하는지 검증
              (setq dist_diff (- dir_dist base_start_dist))
              (if (< dist_diff 0) (setq dist_diff (+ total_bound_dist dist_diff)))
              (if (< dist_diff (/ total_bound_dist 2.0))
                (setq is_forward T)
                (setq is_forward nil)
              )
            )
            (progn
              ;; 열린 선형일 경우 기존 방식 유지 (클릭 위치 기준 양 끝단 거리 비교)
              (setq start_pt_dist (distance (vlax-curve-getStartPoint ent_bound) bound_pick_pt))
              (setq end_pt_dist (distance (vlax-curve-getEndPoint ent_bound) bound_pick_pt))
              (setq base_start_dist 0.0)
              (if (<= start_pt_dist end_pt_dist)
                (setq is_forward T)
                (setq is_forward nil)
              )
            )
          )
          
          ;; ====== 절점(Vertex) 기반 정확한 누적 거리(Chainage) 사전 추출 ======
  (setq end_p (fix (+ (vlax-curve-getEndParam ent_bound) 0.5)))
  (setq v_chain_list nil k 0)
  (while (if is_closed (< k end_p) (<= k end_p))
    (setq cur_dist (vlax-curve-getDistAtParam ent_bound k))
    (if is_closed
      (progn
        (if is_forward
          (setq chain (- cur_dist base_start_dist))
          (setq chain (- base_start_dist cur_dist))
        )
        ;; 오차 보정
        (if (< (abs chain) 0.01) (setq chain 0.0))
        (if (< chain -0.01) (setq chain (+ total_bound_dist chain)))
      )
      (progn
        (setq chain cur_dist)
        (if (not is_forward) (setq chain (- total_bound_dist chain)))
      )
    )
    (setq v_chain_list (append v_chain_list (list chain)))
    (setq k (1+ k))
  )
          (setq v_chain_list (vl-sort v_chain_list '<))
          (if is_closed
            (setq v_chain_list (append v_chain_list (list total_bound_dist)))
          )
          ;; ===============================================================

          ;; 3-1. 캡슐 문자 추출 및 전개도 상단에 재생성
          (defun draw-capsule-shape (center-pt ang-dir ang-perp h-half w-half rad l-name col / p-c1 p-c2 p-c3 p-c4 p-c5 p-c6 p-c7 p-c8)
            (setq p-c1 (polar (polar center-pt ang-perp (- w-half rad)) ang-dir h-half))
            (setq p-c2 (polar (polar center-pt ang-perp w-half) ang-dir (- h-half rad)))
            (setq p-c3 (polar (polar center-pt ang-perp w-half) (+ ang-dir pi) (- h-half rad)))
            (setq p-c4 (polar (polar center-pt ang-perp (- w-half rad)) (+ ang-dir pi) h-half))
            (setq p-c5 (polar (polar center-pt (- ang-perp pi) (- w-half rad)) (+ ang-dir pi) h-half))
            (setq p-c6 (polar (polar center-pt (- ang-perp pi) w-half) (+ ang-dir pi) (- h-half rad)))
            (setq p-c7 (polar (polar center-pt (- ang-perp pi) w-half) ang-dir (- h-half rad)))
            (setq p-c8 (polar (polar center-pt (- ang-perp pi) (- w-half rad)) ang-dir h-half))
            (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                           (cons 8 l-name) (cons 62 col) '(90 . 8) '(70 . 1)
                           ;; 점의 생성 순서가 시계 방향(CW)이므로, 호(Arc)가 바깥쪽을 향하도록 볼록도(Bulge)를 음수로 변경하여 적용
                           (cons 10 (list (car p-c1) (cadr p-c1))) '(42 . -0.41421356) (cons 10 (list (car p-c2) (cadr p-c2))) '(42 . 0.0)
                           (cons 10 (list (car p-c3) (cadr p-c3))) '(42 . -0.41421356) (cons 10 (list (car p-c4) (cadr p-c4))) '(42 . 0.0)
                           (cons 10 (list (car p-c5) (cadr p-c5))) '(42 . -0.41421356) (cons 10 (list (car p-c6) (cadr p-c6))) '(42 . 0.0)
                           (cons 10 (list (car p-c7) (cadr p-c7))) '(42 . -0.41421356) (cons 10 (list (car p-c8) (cadr p-c8))) '(42 . 0.0)))
          )

          ;; 1. 객체를 타입별로 엄격히 분류 (LWPOLYLINE은 캡슐, LINE은 지시선으로 명확히 구분)
          (setq plan_capsules nil k 0 plan_circles nil plan_texts nil plan_lines nil plan_polys nil)
          (while (< k (sslength ss_plan))
            (setq p_ent (ssname ss_plan k) p_data (entget p_ent) p_type (cdr (assoc 0 p_data)) p_layer (cdr (assoc 8 p_data)))
            (cond
              ((and (= p_type "CIRCLE") (wcmatch (strcase p_layer) "_지시")) (setq plan_circles (append plan_circles (list p_data))))
              ((and (wcmatch p_type "*TEXT") (wcmatch (strcase p_layer) "_지시")) (setq plan_texts (append plan_texts (list p_data))))
              ((and (= p_type "LINE") (wcmatch (strcase p_layer) "_지시")) (setq plan_lines (append plan_lines (list p_ent))))
              ((and (= p_type "LWPOLYLINE") (wcmatch (strcase p_layer) "_지시")) (setq plan_polys (append plan_polys (list p_ent))))
            )
            (setq k (1+ k))
          )

          ;; 2. 내부 함수: 기하학적 절점 매칭(Ray-Casting)을 통한 다각형 내부 텍스트 판별
          (defun is-pt-in-poly (pt poly_ent / data pts count i p1 p2 intersect_x)
            (setq data (entget poly_ent) pts nil)
            (foreach item data (if (= (car item) 10) (setq pts (append pts (list (cdr item))))))
            (setq count 0 i 0)
            (while (< i (length pts))
              (setq p1 (nth i pts))
              (if (= i (1- (length pts))) (setq p2 (nth 0 pts)) (setq p2 (nth (1+ i) pts)))
              (if (or (and (<= (cadr p1) (cadr pt)) (> (cadr p2) (cadr pt)))
                      (and (<= (cadr p2) (cadr pt)) (> (cadr p1) (cadr pt))))
                (progn
                  (setq intersect_x (+ (car p1) (* (/ (- (cadr pt) (cadr p1)) (- (cadr p2) (cadr p1))) (- (car p2) (car p1)))))
                  (if (< (car pt) intersect_x) (setq count (1+ count)))
                )
              )
              (setq i (1+ i))
            )
            (= (rem count 2) 1)
          )

          ;; 3. 캡슐(LWPOLYLINE) 안에 있는 텍스트 매칭 및 할당
          (setq poly_text_alist nil)
          (foreach poly_ent plan_polys
            (setq found_txt nil)
            (foreach t_data plan_texts
              (setq a72 (cdr (assoc 72 t_data)) a73 (cdr (assoc 73 t_data)))
              (setq t_pt (if (and a72 a73 (not (and (= a72 0) (= a73 0)))) (cdr (assoc 11 t_data)) (cdr (assoc 10 t_data))))
              (if (is-pt-in-poly t_pt poly_ent)
                (setq found_txt (cdr (assoc 1 t_data)))
              )
            )
            (if found_txt (setq poly_text_alist (append poly_text_alist (list (cons poly_ent found_txt)))))
          )

          ;; 4. 물리적 연결망(Topology) 추적을 통한 최종 모서리 데이터 구축
          (foreach c_data plan_circles
            (setq c_center (cdr (assoc 10 c_data)) c_rad (cdr (assoc 40 c_data)))
            
            ;; 4-1. 원 내부 번호 텍스트 찾기
            (setq c_num 0 min_in 1e9)
            (foreach t_data plan_texts
              (setq a72 (cdr (assoc 72 t_data)) a73 (cdr (assoc 73 t_data)))
              (setq t_pt (if (and a72 a73 (not (and (= a72 0) (= a73 0)))) (cdr (assoc 11 t_data)) (cdr (assoc 10 t_data))))
              (setq dist (distance (list (car c_center) (cadr c_center)) (list (car t_pt) (cadr t_pt))))
              (if (and (<= dist (+ c_rad 1.0)) (< dist min_in))
                (progn (setq min_in dist c_num (atoi (cdr (assoc 1 t_data)))))
              )
            )
            
            ;; 4-2. 원에 맞닿은 선을 타고 연결된 모든 캡슐 탐색
            (if (> c_num 0)
              (progn
                (setq cap_list nil connected_polys nil)
                (setq active_pts (list (list (car c_center) (cadr c_center))))
                (setq changed T avail_lines plan_lines search_rad (+ c_rad 10.0))
                
                (while changed
                  (setq changed nil remain_lines nil)
                  (foreach l_ent avail_lines
                    (setq p_start (vlax-curve-getStartPoint l_ent) p_end (vlax-curve-getEndPoint l_ent))
                    (setq p1 (list (car p_start) (cadr p_start)) p2 (list (car p_end) (cadr p_end)))
                    (setq is_connected nil)
                    
                    ;; 선의 양끝점이 활성화된 기준점과 닿아있는지 검사
                    (foreach apt active_pts
                      (if (equal apt (list (car c_center) (cadr c_center)))
                        ;; 1. 기준점이 원의 중심일 경우
                        (if (or (< (abs (- (distance apt p1) c_rad)) 10.0)
                                (< (abs (- (distance apt p2) c_rad)) 10.0))
                          (setq is_connected T)
                        )
                        ;; 2. 기준점이 캡슐 연결점일 경우
                        (if (or (< (distance apt p1) 5.0) (< (distance apt p2) 5.0))
                          (setq is_connected T)
                        )
                      )
                    )
                    
                    (if is_connected
                      (progn
                        (setq active_pts (append active_pts (list p1 p2)) changed T)
                        
                        ;; 연결된 선이 어떤 캡슐(LWPOLYLINE)에 닿는지 검사하여 그룹에 추가
                        (foreach poly_ent plan_polys
                          (if (not (member poly_ent connected_polys))
                            (progn
                              (setq cpt1 (vlax-curve-getClosestPointTo poly_ent p1))
                              (if (< (distance p1 (list (car cpt1) (cadr cpt1))) 5.0)
                                (progn (setq connected_polys (append connected_polys (list poly_ent))) (setq active_pts (append active_pts (list (list (car cpt1) (cadr cpt1))))))
                              )
                              (setq cpt2 (vlax-curve-getClosestPointTo poly_ent p2))
                              (if (< (distance p2 (list (car cpt2) (cadr cpt2))) 5.0)
                                (progn (setq connected_polys (append connected_polys (list poly_ent))) (setq active_pts (append active_pts (list (list (car cpt2) (cadr cpt2))))))
                              )
                            )
                          )
                        )
                      )
                      (setq remain_lines (append remain_lines (list l_ent)))
                    )
                  )
                  (setq avail_lines remain_lines)
                )
                
                ;; 4-3. 찾은 캡슐 안의 텍스트를 리스트로 정리
                (foreach poly_ent connected_polys
                  (setq txt (cdr (assoc poly_ent poly_text_alist)))
                  (if txt (setq cap_list (append cap_list (list txt))))
                )
                
                ;; 캡슐 번호를 숫자로 변환하여 오름차순 정렬
                (if cap_list
                  (progn
                    (setq cap_list (vl-sort cap_list '(lambda (a b) (< (atoi a) (atoi b)))))
                    (setq plan_capsules (append plan_capsules (list (list c_num cap_list))))
                  )
                )
              )
            )
          )

          ;; ====== 데이터 인식 현황 검증 리포트 출력 ======
          (setq num_vertices (length v_chain_list))
          (setq recognized_max 0 last_corner_pile 0)
          (foreach pc plan_capsules 
            (if (> (car pc) recognized_max) 
              (progn
                (setq recognized_max (car pc))
                (setq last_corner_pile 0)
                (foreach cap_s (cadr pc)
                  (if (> (atoi cap_s) last_corner_pile) (setq last_corner_pile (atoi cap_s)))
                )
              )
            )
          )
          
          (princ "\n\n========================================================")
          (princ "\n[ 모서리 번호 및 경계선 절점 인식 현황 ]")
          (princ (strcat "\n -> 추출된 경계선 절점 개수: " (itoa num_vertices) "개"))
          (princ (strcat "\n -> 평면도에서 인식된 마지막 모서리 번호: " (itoa recognized_max) "번"))
          (princ (strcat "\n -> 해당 모서리(" (itoa recognized_max) "번) 캡슐 내 말뚝 누적 개수: " (itoa last_corner_pile) "개"))
          
          (if (/= num_vertices recognized_max)
            (princ "\n 주의!!! 절점 개수와 캡슐 모서리 번호가 일치하지 않습니다. 매칭 오류나 누락이 발생할 수 있습니다.")
            (princ "\n 일치 확인! 절점 개수와 모서리 번호가 정확히 일치합니다.")
          )
          (princ "\n========================================================\n")
          ;; ===============================================================

          ;; 전개도 상단에 지시선, 캡슐, 상단 지시선 및 번호(원) 작도 (평면도 원본 구조 복원)
          (setq item_idx 0 total_c (length circle_list))
          (foreach item circle_list
            (setq e_num (nth 0 item) e_x (nth 1 item) e_y (nth 2 item) e_rad (nth 3 item))
            (setq match_caps nil)
            (foreach pc plan_capsules (if (= (car pc) e_num) (setq match_caps (cadr pc))))
            
            (if match_caps
              (progn
                (setq caps_to_draw match_caps)
                ;; 폐합선의 경우 전개도 시작점(idx=0)은 상단 캡슐 1개만, 마지막 지점은 수집된 캡슐 전체를 그림
                (if (and is_closed (= e_num 1) (= item_idx 0))
                  (setq caps_to_draw (list (car match_caps)))
                )
                
                ;; 모서리번호(원)의 X좌표를 앞서 추출한 "절점 자체의 정확한 누적 거리(Chainage)"로 덮어쓰기
                (setq v_idx (1- e_num))
                (if (and is_closed (= item_idx (1- total_c)))
                  (setq v_idx (1- (length v_chain_list)))
                )
                (setq exact_chain (nth v_idx v_chain_list))
                (if exact_chain 
                  (setq e_x (+ prof_start_x exact_chain))
                )
                
                ;; 0. 기준 좌표 설정 (기존 전개도 번호 원의 가장 위쪽 끝점)
                (setq base_y (+ e_y e_rad))
                (setq cur_y base_y)
                
                ;; 1. 첫 번째 빨간색 지시선 (길이 1300)
                (setq red_line_start (list e_x cur_y 0.0))
                (setq cur_y (+ cur_y 1300.0))
                (setq red_line_end (list e_x cur_y 0.0))
                (entmake (list '(0 . "LINE") '(8 . "_지시") '(62 . 1) (cons 10 red_line_start) (cons 11 red_line_end)))
                
                ;; 2. 캡슐 및 캡슐 텍스트 동적 다중 생성 (아래에서 위로 적층)
                (setq rev_caps (reverse caps_to_draw))
                (setq cap_idx 0 num_caps (length rev_caps))
                (setq cap_h_half 600.0 cap_c_rad 480.0)
                
                (foreach cap_str rev_caps
                  (setq cap_center_y (+ cur_y cap_h_half))
                  (setq cap_center (list e_x cap_center_y 0.0))
                  (setq cap_w_half (+ cap_c_rad (* (strlen cap_str) 300.0)))
                  (draw-capsule-shape cap_center (/ pi 2.0) 0.0 cap_h_half cap_w_half cap_c_rad "_지시" 1)
                  
                  (entmake (list '(0 . "TEXT") '(8 . "_지시") '(62 . 7) (cons 10 cap_center) (cons 11 cap_center)
                                 '(40 . 720.0) (cons 50 0.0) (cons 1 cap_str) 
                                 '(7 . "Standard") '(72 . 1) '(73 . 2)))
                  
                  (setq cur_y (+ cur_y (* cap_h_half 2.0)))
                  
                  ;; 캡슐과 캡슐 사이 연결선 (빨간색 600)
                  (if (< cap_idx (1- num_caps))
                    (progn
                      (setq red_short_start (list e_x cur_y 0.0))
                      (setq cur_y (+ cur_y 600.0))
                      (setq red_short_end (list e_x cur_y 0.0))
                      (entmake (list '(0 . "LINE") '(8 . "_지시") '(62 . 1) (cons 10 red_short_start) (cons 11 red_short_end)))
                    )
                  )
                  (setq cap_idx (1+ cap_idx))
                )
                
                ;; 3. 노란색 지시선 (길이 1300, 최상단 캡슐 위에서 시작)
                (setq yel_line_start (list e_x cur_y 0.0))
                (setq cur_y (+ cur_y 1300.0))
                (setq yel_line_end (list e_x cur_y 0.0))
                (entmake (list '(0 . "LINE") '(8 . "_지시") '(62 . 2) (cons 10 yel_line_start) (cons 11 yel_line_end)))
                
                ;; 4. 지시번호 노란색 원 및 내부 숫자
                (setq top_circle_rad 1200.0)
                (setq top_circle_center (list e_x (+ cur_y top_circle_rad) 0.0))
                (entmake (list '(0 . "CIRCLE") '(8 . "_지시") '(62 . 2) (cons 10 top_circle_center) (cons 40 top_circle_rad)))
                
                (entmake (list '(0 . "TEXT") '(8 . "_지시") '(62 . 7) (cons 10 top_circle_center) (cons 11 top_circle_center)
                               '(40 . 840.0) (cons 50 0.0) (cons 1 (itoa e_num)) 
                               '(7 . "Standard") '(72 . 1) '(73 . 2)))
              )
            )
            (setq item_idx (1+ item_idx))
          )

          ;; 블록 이름에서 규격을 스캔하여 폭(Width) 값을 반환하는 함수
          (defun get-pile-size-from-name (blk_name / upper_name)
            (setq upper_name (strcase blk_name))
            (cond
              ((vl-string-search "250" upper_name) 250.0)
              ((or (vl-string-search "298" upper_name) (vl-string-search "201" upper_name)) 201.0)
              ((vl-string-search "300" upper_name) 300.0)
              ((vl-string-search "350" upper_name) 350.0)
              ((vl-string-search "400" upper_name) 400.0)
              (t nil) ; 매칭되는 규격이 없을 경우 nil 반환
            )
          )

          ;; 3-2. 측면말뚝 X좌표 및 규격 사전 추출 (A방식 전용)
          (setq raw_piles_info nil) ; (X좌표 추출된규격) 리스트 형태로 저장
          (if ss_plan
            (progn
              (setq k 0)
              (while (< k (sslength ss_plan))
                (setq p_ent (ssname ss_plan k) p_data (entget p_ent))
                (if (= (cdr (assoc 0 p_data)) "INSERT")
                  (progn
                    ;; 블록 이름 가져와서 규격 추출 시도
                    (setq blk_name (cdr (assoc 2 p_data)))
                    (setq ext_size (if blk_name (get-pile-size-from-name blk_name) nil))

                    (setq ins_pt (cdr (assoc 10 p_data)))
                    (setq cpt (vlax-curve-getClosestPointTo ent_bound ins_pt))
                    (if is_closed
                      (progn
                        (setq cur_dist (vlax-curve-getDistAtPoint ent_bound cpt))
                        (if is_forward
                          (setq chain (- cur_dist base_start_dist))
                          (setq chain (- base_start_dist cur_dist))
                        )
                        (if (< chain 0) (setq chain (+ total_bound_dist chain)))
                      )
                      (progn
                        (setq chain (vlax-curve-getDistAtPoint ent_bound cpt))
                        (if (not is_forward) (setq chain (- total_bound_dist chain)))
                      )
                    )
                    ;; X좌표와 추출된 규격을 함께 저장
                    (setq raw_piles_info (append raw_piles_info (list (list (+ prof_start_x chain) ext_size))))
                  )
                )
                (setq k (1+ k))
              )
              
              ;; ssget의 무작위 선택 순서로 인한 꼬임을 방지하기 위해 X좌표 기준으로 오름차순 정렬
              (setq raw_piles_info (vl-sort raw_piles_info '(lambda (a b) (< (car a) (car b)))))
            )
          )
        )
        (progn
          ;; ss_plan 미선택 시 (B방식)
          (princ "\n수동(C.T.C) 입력 모드로 진행합니다.")
        )
      )

      ;;;;==========================================================================
      ;;;; 4. 통합 구간별 실시간 작도 및 경계 말뚝(중복) 안전측 덮어쓰기 로직
      ;;;;==========================================================================
      (setq is_loop_done nil)
      (setq current_target_num (car (nth 0 circle_list)))
      (setq prev_section_max_x (cadr (nth 0 circle_list)))
      (setq final_target_x (cadr (last circle_list)))
      (setq drawn_piles_info nil) ; 작도된 말뚝 기록: ( (x top_y bot_y pw poly_ent line_ent ghost_flag) ... )
      (setq last_pile_x prev_section_max_x)
      
      (create_layer "_측면말뚝" 3)
      
      (while (not is_loop_done)
        (princ (strcat "\n[" (itoa current_target_num) "번] 이후부터 적용할 구간의 번호(원)를 드래그하여 선택하세요: "))
        (setq ss_sec (ssget '((0 . "CIRCLE") (8 . "_joint,_바닥번호"))))
        (if (null ss_sec) (progn (alert "구간이 선택되지 않았습니다.") (restore_sys) (exit)))
        
        (setq sec_nums nil sec_x_list nil j 0)
        (while (< j (sslength ss_sec))
          (setq c_ent (ssname ss_sec j))
          (setq c_center (cdr (assoc 10 (entget c_ent))))
          (setq num (get-num-from-circle c_ent))
          (if (> num 0)
            (progn
              (setq sec_nums (append sec_nums (list num)))
              (setq sec_x_list (append sec_x_list (list (car c_center))))
            )
          )
          (setq j (1+ j))
        )
        
        (setq sec_end_x (apply 'max sec_x_list))
        (setq min_sel_x (apply 'min sec_x_list))
        
        ;; 선택한 구간의 시작점이 이전 구간의 끝점보다 앞서 있으면 '수정'으로 간주
        (if (< min_sel_x (- prev_section_max_x 1.0))
          (progn
            (setq sec_start_x min_sel_x)
            (princ (strcat "\n[알림] 이전 구간을 포함하여 선택했습니다. 겹치는 구간의 기존 말뚝을 삭제하고 재작성합니다."))
            
            ;; 겹치는 구간의 기존 말뚝(경계점 제외, 내부 말뚝들)을 도면과 리스트에서 완전 삭제
            (setq temp_list nil e_idx 0)
            (while (< e_idx (length drawn_piles_info))
              (setq ep (nth e_idx drawn_piles_info))
              (setq ep_x (car ep))
              ;; 시작 경계점(sec_start_x)은 안전측 비교를 위해 남겨두고, 그 이후부터 이전 끝점까지 완전 삭제
              (if (and (> ep_x (+ sec_start_x 1.0)) (<= ep_x (+ prev_section_max_x 1.0)))
                (progn
                  (if (nth 4 ep) (entdel (nth 4 ep)))
                  (if (nth 5 ep) (entdel (nth 5 ep)))
                )
                (setq temp_list (append temp_list (list ep)))
              )
              (setq e_idx (1+ e_idx))
            )
            (setq drawn_piles_info temp_list)
          )
          (setq sec_start_x prev_section_max_x)
        )
        
        ;; 다음 프롬프트를 위한 번호 갱신
        (foreach item circle_list
           (if (equal (cadr item) sec_end_x 1.0) (setq current_target_num (car item)))
        )
        
        ;; 해당 구간의 제원 입력
        (setq depth_in (getreal "\n근입깊이(m) 입력 <4.0>: "))
        (if (null depth_in) (setq depth_in 4.0))
        (setq cur_depth (* depth_in 1000.0))

        ;; 현재 구간의 말뚝 데이터를 필터링하고 규격 프롬프트가 필요한지(누락된 규격이 있는지) 검사
        (setq cur_piles_info nil)
        (setq need_size_prompt nil)
        
        (if ss_plan
          (progn
            ;; A방식: 추출된 전체 정보 중 현재 구간 내에 있는 말뚝 선별
            (foreach p_info raw_piles_info
              (setq px (car p_info) pw (cadr p_info))
              (if (and (>= px (- sec_start_x 1.0)) (<= px (+ sec_end_x 1.0)))
                (progn
                  (setq cur_piles_info (append cur_piles_info (list p_info)))
                  ;; 추출된 규격이 단 하나라도 없다면 프롬프트 활성화
                  (if (null pw) (setq need_size_prompt T))
                )
              )
            )
            ;; 구간 내에 캡슐(말뚝)이 아예 없는 경우에도 안전장치로 프롬프트 활성화
            (if (null cur_piles_info) (setq need_size_prompt T))
          )
          (progn
            ;; B방식: 블록이 없으므로 무조건 규격 입력 필요
            (setq need_size_prompt T)
          )
        )

        ;; 기본 규격 설정 (필요할 때만 사용자에게 물어봄)
        (setq def_pw 300.0)
        (if need_size_prompt
          (progn
            (initget "1 1:H250 2 2:H298x201 3 3:H300 4 4:H350 5 5:H400")
            (setq size_opt (getkword "\n측면말뚝 규격 [1:H250 / 2:H298x201 / 3:H300 / 4:H350 / 5:H400] <3>: "))
            (cond
              ((or (= size_opt "1") (= size_opt "1:H250")) (setq def_pw 250.0))
              ((or (= size_opt "2") (= size_opt "2:H298x201")) (setq def_pw 201.0))
              ((or (= size_opt "4") (= size_opt "4:H350")) (setq def_pw 350.0))
              ((or (= size_opt "5") (= size_opt "5:H400")) (setq def_pw 400.0))
              (t (setq def_pw 300.0))
            )
          )
        )
        
        (if (not ss_plan)
          (progn
            ;; B방식: 수동 C.T.C 좌표 생성 및 입력된 기본 규격(def_pw) 일괄 적용
            (setq ctc_in (getreal "\nC.T.C(m) 입력 <1.5>: "))
            (if (null ctc_in) (setq ctc_in 1.5))
            (setq ctc (* ctc_in 1000.0))
            
            (setq last_pile_x sec_start_x)
            (setq cur_piles_info (append cur_piles_info (list (list sec_start_x def_pw))))
            (setq curr_x (+ last_pile_x ctc))
            (while (< curr_x (- sec_end_x 1.0))
              (setq cur_piles_info (append cur_piles_info (list (list curr_x def_pw))))
              (setq last_pile_x curr_x)
              (setq curr_x (+ last_pile_x ctc))
            )
            (setq cur_piles_info (append cur_piles_info (list (list sec_end_x def_pw))))
          )
        )
        
        ;; 구간 내 말뚝 실시간 작도 및 중복 덮어쓰기 판별
        (foreach p_info cur_piles_info
          (setq px (car p_info))
          (setq ext_pw (cadr p_info))
          ;; 추출된 규격이 있으면 우선 사용하고, 없으면 기본값(def_pw) 사용
          (setq cur_pw (if ext_pw ext_pw def_pw)) 
          
          (setq top_y (get-y-on-pline ent_grnd px))
          (setq bot_y (- (get-y-on-pline ent_excav px) cur_depth))
          
          (if (and top_y bot_y)
            (progn
              ;; 해당 X좌표에 이미 작도된 말뚝이 있는지 검색
              (setq existing_pile nil e_idx 0 found_idx -1)
              (while (< e_idx (length drawn_piles_info))
                (setq ep (nth e_idx drawn_piles_info))
                (if (< (abs (- (car ep) px)) 1.0)
                  (progn (setq existing_pile ep) (setq found_idx e_idx))
                )
                (setq e_idx (1+ e_idx))
              )
              
              (if existing_pile
                (progn
                  ;; 중복 발견 시: 기존 제원과 신규 제원을 비교하여 안전측(최대/최심) 도출
                  (setq old_top_y (nth 1 existing_pile))
                  (setq old_bot_y (nth 2 existing_pile))
                  (setq old_pw (nth 3 existing_pile))
                  (setq old_poly (nth 4 existing_pile))
                  (setq old_line (nth 5 existing_pile))
                  (setq old_ghost (nth 6 existing_pile))
                  
                  (setq final_bot_y (min bot_y old_bot_y))
                  (setq final_pw (max cur_pw old_pw))
                  
                  ;; 새로운 제원이 기존보다 크거나 깊을 경우에만 도면 업데이트 수행
                  (if (or (< final_bot_y (- old_bot_y 1.0)) (> final_pw (+ old_pw 1.0)))
                    (progn
                      ;; 캐드 도면에서 기존 객체 완전 삭제
                      (if (and old_poly (entget old_poly)) (entdel old_poly))
                      (if (and old_line (entget old_line)) (entdel old_line))
                      
                      ;; 새롭고 큰 안전측 객체로 재작도
                      (setq half_w (/ final_pw 2.0))
                      (setq pt1 (list (- px half_w) final_bot_y) pt2 (list (+ px half_w) final_bot_y)
                            pt3 (list (+ px half_w) old_top_y) pt4 (list (- px half_w) old_top_y))
                            
                      (setq new_poly (entmakex (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1) '(8 . "_측면말뚝") (cons 10 pt1) (cons 10 pt2) (cons 10 pt3) (cons 10 pt4))))
                      (setq new_line (entmakex (list '(0 . "LINE") '(8 . "_측면말뚝") (cons 10 (list px final_bot_y)) (cons 11 (list px old_top_y)))))
                      
                      ;; 메모리 리스트 업데이트
                      (setq new_ep (list px old_top_y final_bot_y final_pw new_poly new_line old_ghost))
                      (setq temp_list nil e_idx 0)
                      (while (< e_idx (length drawn_piles_info))
                        (if (= e_idx found_idx)
                          (setq temp_list (append temp_list (list new_ep)))
                          (setq temp_list (append temp_list (list (nth e_idx drawn_piles_info))))
                        )
                        (setq e_idx (1+ e_idx))
                      )
                      (setq drawn_piles_info temp_list)
                    )
                  )
                )
                (progn
                  ;; 신규 위치일 경우 즉시 작도
                  (setq half_w (/ cur_pw 2.0))
                  (setq pt1 (list (- px half_w) bot_y) pt2 (list (+ px half_w) bot_y)
                        pt3 (list (+ px half_w) top_y) pt4 (list (- px half_w) top_y))
                        
                  (setq new_poly (entmakex (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1) '(8 . "_측면말뚝") (cons 10 pt1) (cons 10 pt2) (cons 10 pt3) (cons 10 pt4))))
                  (setq new_line (entmakex (list '(0 . "LINE") '(8 . "_측면말뚝") (cons 10 (list px bot_y)) (cons 11 (list px top_y)))))
                  
                  (setq drawn_piles_info (append drawn_piles_info (list (list px top_y bot_y cur_pw new_poly new_line nil))))
                )
              )
            )
          )
        )
        
        (setq prev_section_max_x sec_end_x)
        ;; 종료 조건 달성 검사
        (if (>= sec_end_x (- final_target_x 1.0))
          (setq is_loop_done T)
        )
      )

      ;; 폐합선인 경우 마지막 위치(번호 1번 자리)에 추가할 허수(Ghost) 말뚝 추출/생성
      (if is_closed
        (progn
          (setq end_circle_x (cadr (last circle_list)))
          (setq ghost_exists nil)
          (foreach ep drawn_piles_info
            (if (< (abs (- (car ep) end_circle_x)) 1.0)
              (setq ghost_exists T)
            )
          )
          
          (if (not ghost_exists)
            (progn
              ;; 고스트 말뚝이 아직 작도되지 않았으면, 1번 말뚝 제원을 복사하여 8번 색상으로 새로 작도
              (setq first_p nil)
              (foreach ep drawn_piles_info (if (null first_p) (setq first_p ep)))
              (if first_p
                (progn
                  (setq g_bot_y (nth 2 first_p) g_pw (nth 3 first_p) g_top_y (get-y-on-pline ent_grnd end_circle_x))
                  (setq half_w (/ g_pw 2.0))
                  (setq pt1 (list (- end_circle_x half_w) g_bot_y) pt2 (list (+ end_circle_x half_w) g_bot_y)
                        pt3 (list (+ end_circle_x half_w) g_top_y) pt4 (list (- end_circle_x half_w) g_top_y))
                  
                  (setq g_poly (entmakex (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1) '(8 . "_측면말뚝") '(62 . 8) (cons 10 pt1) (cons 10 pt2) (cons 10 pt3) (cons 10 pt4))))
                  (setq g_line (entmakex (list '(0 . "LINE") '(8 . "_측면말뚝") '(62 . 8) (cons 10 (list end_circle_x g_bot_y)) (cons 11 (list end_circle_x g_top_y)))))
                  (setq drawn_piles_info (append drawn_piles_info (list (list end_circle_x g_top_y g_bot_y g_pw g_poly g_line T))))
                )
              )
            )
            (progn
              ;; 이미 끝점에 말뚝이 존재한다면 도면층 색상을 8번(회색)으로 변경하고 Ghost(T) 플래그만 설정
              (setq e_idx 0 temp_list nil)
              (while (< e_idx (length drawn_piles_info))
                (setq ep (nth e_idx drawn_piles_info))
                (if (< (abs (- (car ep) end_circle_x)) 1.0)
                  (progn
                    (setq ep_poly (nth 4 ep) ep_line (nth 5 ep))
                    (if ep_poly (entmod (append (entget ep_poly) (list '(62 . 8)))))
                    (if ep_line (entmod (append (entget ep_line) (list '(62 . 8)))))
                    (setq temp_list (append temp_list (list (list (car ep) (nth 1 ep) (nth 2 ep) (nth 3 ep) ep_poly ep_line T))))
                  )
                  (setq temp_list (append temp_list (list ep)))
                )
                (setq e_idx (1+ e_idx))
              )
              (setq drawn_piles_info temp_list)
            )
          )
        )
      )
      
      (setq final_piles nil)
      (foreach ep drawn_piles_info
        ;; Ghost(T)로 분류된 꼬리 허수 말뚝을 제외하고 순수 유효 말뚝만 리포트 집계용으로 추출
        (if (not (nth 6 ep))
          (setq final_piles (append final_piles (list ep)))
        )
      )
      
      ;;;;==========================================================================
      ;;;; 10번째 측면말뚝마다 하단에 캡슐 번호 생성
      ;;;;==========================================================================
      ;; 캡슐 작도 전용 내부 함수
      (defun draw-bottom-capsule (center-pt h-half w-half rad l-name / p-c1 p-c2 p-c3 p-c4 p-c5 p-c6 p-c7 p-c8)
        (setq p-c1 (list (+ (car center-pt) (- w-half rad)) (+ (cadr center-pt) h-half)))
        (setq p-c2 (list (+ (car center-pt) w-half) (+ (cadr center-pt) (- h-half rad))))
        (setq p-c3 (list (+ (car center-pt) w-half) (- (cadr center-pt) (- h-half rad))))
        (setq p-c4 (list (+ (car center-pt) (- w-half rad)) (- (cadr center-pt) h-half)))
        (setq p-c5 (list (- (car center-pt) (- w-half rad)) (- (cadr center-pt) h-half)))
        (setq p-c6 (list (- (car center-pt) w-half) (- (cadr center-pt) (- h-half rad))))
        (setq p-c7 (list (- (car center-pt) w-half) (+ (cadr center-pt) (- h-half rad))))
        (setq p-c8 (list (- (car center-pt) (- w-half rad)) (+ (cadr center-pt) h-half)))
        (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                       (cons 8 l-name) '(62 . 1) '(90 . 8) '(70 . 1)
                       (cons 10 p-c1) '(42 . -0.41421356) (cons 10 p-c2) '(42 . 0.0)
                       (cons 10 p-c3) '(42 . -0.41421356) (cons 10 p-c4) '(42 . 0.0)
                       (cons 10 p-c5) '(42 . -0.41421356) (cons 10 p-c6) '(42 . 0.0)
                       (cons 10 p-c7) '(42 . -0.41421356) (cons 10 p-c8) '(42 . 0.0)))
      )

      (setq pile_idx 1)
      (foreach ep final_piles
        ;; 10의 배수(10, 20, 30...)번째 말뚝인지 검사
        (if (= (rem pile_idx 10) 0)
          (progn
            (setq p_x (car ep) p_bot_y (nth 2 ep))
            
            ;; 1. 지시선 생성
            (setq gap 1000.0 l_len 1000.0)
            (setq l_start (list p_x (- p_bot_y gap) 0.0))
            (setq l_end (list p_x (- p_bot_y (+ gap l_len)) 0.0))
            (entmake (list '(0 . "LINE") '(8 . "_지시") '(62 . 1) (cons 10 l_start) (cons 11 l_end)))
            
            ;; 2. 텍스트 속성 (E모드에서 미지정 시 도면 설정값 또는 600 사용)
            (setq cur_txt_hgt (if txt_hgt txt_hgt (if (vlax-ldata-get "DRG_SETTINGS" "TXT_HGT") (vlax-ldata-get "DRG_SETTINGS" "TXT_HGT") 600.0)))
            (setq cur_txt_sty (if txt_sty txt_sty (if (vlax-ldata-get "DRG_SETTINGS" "TXT_STY") (vlax-ldata-get "DRG_SETTINGS" "TXT_STY") "Standard")))

            ;; 3. 캡슐 생성
            (setq cap_h_half (* cur_txt_hgt (/ 500.0 600.0)))
            (setq cap_rad (* cur_txt_hgt (/ 400.0 600.0)))
            (setq cap_w_half (* (+ (* cur_txt_hgt 2.0) (* (strlen (itoa pile_idx)) (* cur_txt_hgt 0.5))) 0.5))
            
            ;; 캡슐의 중심 Y좌표
            (setq cap_center (list p_x (- (cadr l_end) cap_h_half) 0.0))
            (draw-bottom-capsule cap_center cap_h_half cap_w_half cap_rad "_지시")
            
            ;; 4. 내부 텍스트 생성
            (entmake (list '(0 . "TEXT") '(8 . "_지시") '(62 . 7) 
                           (cons 10 cap_center) (cons 11 cap_center)
                           (cons 40 cur_txt_hgt) (cons 50 0.0) (cons 1 (itoa pile_idx)) 
                           (cons 7 cur_txt_sty) '(72 . 1) '(73 . 2)))
          )
        )
        (setq pile_idx (1+ pile_idx))
      )
      
     ;;;;==========================================================================
      ;;;; 5. 평면도 매칭 결과 검증 리포트 (Validation)
      ;;;;==========================================================================
      (princ "\n\n========================================================")
      (if (and ss_plan plan_capsules)
        (progn
          (setq max_cap_num 0)
          (foreach pc plan_capsules
            (foreach cap_s (cadr pc)
              (if (> (atoi cap_s) max_cap_num) (setq max_cap_num (atoi cap_s)))
            )
          )
          (setq final_drawn_count (length final_piles))
          (princ (strcat "\n-> 전개도에 작도된 총 측면말뚝 개수: " (itoa final_drawn_count)))
          (princ (strcat "\n-> 평면도에서 인식된 마지막 캡슐 숫자: " (itoa max_cap_num)))
          (if (/= final_drawn_count max_cap_num)
             (princ "\n주의!!! 전개도에 작도된 측면말뚝 개수와 캡슐 내 숫자가 불일치합니다.")
             (princ "\n완벽합니다! 전개도 작도 및 캡슐 검증이 정확히 일치합니다.")
          )
        )
        ;; 만약 추출에 또 실패할 경우 침묵하지 않고 사용자에게 원인을 알리도록 개선
        (princ "\n주의: 평면도 객체에서 캡슐 데이터를 추출하지 못해 검증 및 캡슐 작도가 생략되었습니다.")
      )
      (princ "\n========================================================")
      
      (princ "\n전개도 작성이 완료되었습니다.")
    )
    ) ; [전개도작성(E) 모드 끝]
  ) ; main_opt cond 종료
  
  (restore_sys)
  (princ)
)

;; =========================================================================
;; [TSP <-> DRG 연동 전용 모듈 (전개도 해치 + 띠장 + 지보재 블록 통합)]
;; =========================================================================

;; --- [1] 지보재 블록 5종 자동 등록 함수 (My_Block_Library.lsp 내장) ---
(defun drg-ensure-support-blocks ( / )
  (if (not (tblsearch "LAYER" "_사보강재(ConerStrut)"))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") '(2 . "_사보강재(ConerStrut)") '(70 . 0) '(62 . 3) '(6 . "Continuous")))
  )
  (if (not (tblsearch "LAYER" "_앵커(Anchor)"))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") '(2 . "_앵커(Anchor)") '(70 . 0) '(62 . 6) '(6 . "Continuous")))
  )
  (if (not (tblsearch "LAYER" "_버팀보(Strut)"))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") '(2 . "_버팀보(Strut)") '(70 . 0) '(62 . 3) '(6 . "Continuous")))
  )

  ;; 1. 사보강재(ConerStrut)_H300x300
  (if (not (tblsearch "BLOCK" "사보강재(ConerStrut)_H300x300"))
    (progn
      (entmake (list '(0 . "BLOCK") '(100 . "AcDbEntity") '(8 . "0") '(100 . "AcDbBlockBegin") '(2 . "사보강재(ConerStrut)_H300x300") '(70 . 0) '(10 0.0 0.0 0.0)))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "_사보강재(ConerStrut)") '(100 . "AcDbPolyline") '(90 . 3) '(70 . 1) '(10 0.0 300.0) '(10 0.0 0.0) '(10 150.0 150.0)))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "_사보강재(ConerStrut)") '(100 . "AcDbPolyline") '(90 . 3) '(70 . 1) '(10 300.0 300.0) '(10 300.0 0.0) '(10 150.0 150.0)))
      (entmake (list '(0 . "HATCH") '(100 . "AcDbEntity") '(8 . "_사보강재(ConerStrut)") '(6 . "SOLID") '(100 . "AcDbHatch") '(10 0.0 0.0 0.0) '(210 0.0 0.0 1.0) '(2 . "SOLID") '(70 . 1) '(71 . 0) '(91 . 1) '(92 . 2) '(72 . 0) '(73 . 1) '(93 . 3) '(10 0.0 300.0 0.0) '(10 0.0 0.0 0.0) '(10 150.0 150.0 0.0) '(97 . 0) '(75 . 0) '(76 . 1) '(98 . 0)))
      (entmake (list '(0 . "HATCH") '(100 . "AcDbEntity") '(8 . "_사보강재(ConerStrut)") '(6 . "SOLID") '(100 . "AcDbHatch") '(10 0.0 0.0 0.0) '(210 0.0 0.0 1.0) '(2 . "SOLID") '(70 . 1) '(71 . 0) '(91 . 1) '(92 . 2) '(72 . 0) '(73 . 1) '(93 . 3) '(10 300.0 300.0 0.0) '(10 300.0 0.0 0.0) '(10 150.0 150.0 0.0) '(97 . 0) '(75 . 0) '(76 . 1) '(98 . 0)))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "_사보강재(ConerStrut)") '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1) '(10 0.0 0.0) '(10 300.0 0.0) '(10 300.0 300.0) '(10 0.0 300.0)))
      (entmake '((0 . "ENDBLK") (100 . "AcDbEntity") (8 . "0") (100 . "AcDbBlockEnd")))
    )
  )

  ;; 2. 사보강재(ConerStrut)_2H300x300
  (if (not (tblsearch "BLOCK" "사보강재(ConerStrut)_2H300x300"))
    (progn
      (entmake (list '(0 . "BLOCK") '(100 . "AcDbEntity") '(8 . "0") '(100 . "AcDbBlockBegin") '(2 . "사보강재(ConerStrut)_2H300x300") '(70 . 0) '(10 0.0 0.0 0.0)))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "_사보강재(ConerStrut)") '(100 . "AcDbPolyline") '(90 . 3) '(70 . 1) '(10 600.0 300.0) '(10 600.0 0.0) '(10 750.0 150.0)))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "_사보강재(ConerStrut)") '(100 . "AcDbPolyline") '(90 . 3) '(70 . 1) '(10 900.0 300.0) '(10 900.0 0.0) '(10 750.0 150.0)))
      (entmake (list '(0 . "HATCH") '(100 . "AcDbEntity") '(8 . "_사보강재(ConerStrut)") '(100 . "AcDbHatch") '(10 0.0 0.0 0.0) '(210 0.0 0.0 1.0) '(2 . "SOLID") '(70 . 1) '(71 . 0) '(91 . 1) '(92 . 2) '(72 . 0) '(73 . 1) '(93 . 3) '(10 600.0 300.0 0.0) '(10 600.0 0.0 0.0) '(10 750.0 150.0 0.0) '(97 . 0) '(75 . 0) '(76 . 1) '(98 . 0)))
      (entmake (list '(0 . "HATCH") '(100 . "AcDbEntity") '(8 . "_사보강재(ConerStrut)") '(100 . "AcDbHatch") '(10 0.0 0.0 0.0) '(210 0.0 0.0 1.0) '(2 . "SOLID") '(70 . 1) '(71 . 0) '(91 . 1) '(92 . 2) '(72 . 0) '(73 . 1) '(93 . 3) '(10 900.0 300.0 0.0) '(10 900.0 0.0 0.0) '(10 750.0 150.0 0.0) '(97 . 0) '(75 . 0) '(76 . 1) '(98 . 0)))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "_사보강재(ConerStrut)") '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1) '(10 0.0 300.0) '(10 900.0 300.0) '(10 900.0 0.0) '(10 0.0 0.0)))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "_사보강재(ConerStrut)") '(100 . "AcDbPolyline") '(90 . 3) '(70 . 1) '(10 0.0 300.0) '(10 0.0 0.0) '(10 150.0 150.0)))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "_사보강재(ConerStrut)") '(100 . "AcDbPolyline") '(90 . 3) '(70 . 1) '(10 300.0 300.0) '(10 300.0 0.0) '(10 150.0 150.0)))
      (entmake (list '(0 . "HATCH") '(100 . "AcDbEntity") '(8 . "_사보강재(ConerStrut)") '(100 . "AcDbHatch") '(10 0.0 0.0 0.0) '(210 0.0 0.0 1.0) '(2 . "SOLID") '(70 . 1) '(71 . 0) '(91 . 1) '(92 . 2) '(72 . 0) '(73 . 1) '(93 . 3) '(10 0.0 300.0 0.0) '(10 0.0 0.0 0.0) '(10 150.0 150.0 0.0) '(97 . 0) '(75 . 0) '(76 . 1) '(98 . 0)))
      (entmake (list '(0 . "HATCH") '(100 . "AcDbEntity") '(8 . "_사보강재(ConerStrut)") '(100 . "AcDbHatch") '(10 0.0 0.0 0.0) '(210 0.0 0.0 1.0) '(2 . "SOLID") '(70 . 1) '(71 . 0) '(91 . 1) '(92 . 2) '(72 . 0) '(73 . 1) '(93 . 3) '(10 300.0 300.0 0.0) '(10 300.0 0.0 0.0) '(10 150.0 150.0 0.0) '(97 . 0) '(75 . 0) '(76 . 1) '(98 . 0)))
      (entmake '((0 . "ENDBLK") (100 . "AcDbEntity") (8 . "0") (100 . "AcDbBlockEnd")))
    )
  )

  ;; 3. 앵커(Anchor)
  (if (not (tblsearch "BLOCK" "앵커(Anchor)"))
    (progn
      (entmake (list '(0 . "BLOCK") '(100 . "AcDbEntity") '(8 . "0") '(100 . "AcDbBlockBegin") '(2 . "앵커(Anchor)") '(70 . 0) '(10 0.0 0.0 0.0)))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "_앵커(Anchor)") '(100 . "AcDbPolyline") '(90 . 5) '(70 . 0) '(10 0.0 0.0) '(10 270.0 0.0) '(10 270.0 650.0) '(10 0.0 650.0) '(10 0.0 0.0)))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "_앵커(Anchor)") '(100 . "AcDbPolyline") '(90 . 5) '(70 . 0) '(10 261.934573 451.934573) '(10 8.065427 451.934573) '(10 8.065427 198.065427) '(10 261.934573 198.065427) '(10 261.934573 451.934573)))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "_앵커(Anchor)") '(100 . "AcDbPolyline") '(90 . 3) '(70 . 0) '(43 . 100.0) '(10 85.000001 325.0) '(40 . 100.0) '(41 . 100.0) '(42 . 1.00000001) '(10 185.0 325.0) '(40 . 100.0) '(41 . 100.0) '(42 . 1.00000001) '(10 85.000001 325.0) '(40 . 100.0) '(41 . 100.0) '(42 . 0.0)))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "_앵커(Anchor)") '(100 . "AcDbPolyline") '(90 . 2) '(70 . 1) '(43 . 100.0) '(10 185.0 325.0) '(40 . 100.0) '(41 . 100.0) '(42 . 1.0) '(10 85.0 325.0) '(40 . 100.0) '(41 . 100.0) '(42 . 1.0)))
      (entmake '((0 . "ENDBLK") (100 . "AcDbEntity") (8 . "0") (100 . "AcDbBlockEnd")))
    )
  )

  ;; 4. 버팀보(Strut)_H300x300
  (if (not (tblsearch "BLOCK" "버팀보(Strut)_H300x300"))
    (progn
      (entmake (list '(0 . "BLOCK") '(100 . "AcDbEntity") '(8 . "0") '(100 . "AcDbBlockBegin") '(2 . "버팀보(Strut)_H300x300") '(70 . 0) '(10 0.0 0.0 0.0)))
      (entmake (list '(0 . "HATCH") '(100 . "AcDbEntity") '(8 . "_버팀보(Strut)") '(100 . "AcDbHatch") '(10 0.0 0.0 0.0) '(210 0.0 0.0 1.0) '(2 . "SOLID") '(70 . 1) '(71 . 0) '(91 . 1) '(92 . 1) '(93 . 4) '(72 . 1) '(10 0.0 0.0 0.0) '(11 0.0 300.0 0.0) '(72 . 1) '(10 0.0 300.0 0.0) '(11 300.0 300.0 0.0) '(72 . 1) '(10 300.0 300.0 0.0) '(11 300.0 0.0 0.0) '(72 . 1) '(10 300.0 0.0 0.0) '(11 0.0 0.0 0.0) '(97 . 0) '(75 . 1) '(76 . 1) '(98 . 1) '(10 0.0 0.0 0.0) '(450 . 0) '(451 . 0) '(460 . 0.0) '(461 . 0.0) '(452 . 0) '(462 . 0.0) '(453 . 2) '(463 . 0.0) '(63 . 5) '(421 . 255) '(463 . 1.0) '(63 . 2) '(421 . 16776960) '(470 . "LINEAR")))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "_버팀보(Strut)") '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1) '(10 0.0 0.0) '(10 0.0 300.0) '(10 300.0 300.0) '(10 300.0 0.0)))
      (entmake '((0 . "ENDBLK") (100 . "AcDbEntity") (8 . "0") (100 . "AcDbBlockEnd")))
    )
  )

  ;; 5. 버팀보(Strut)_2H300x300
  (if (not (tblsearch "BLOCK" "버팀보(Strut)_2H300x300"))
    (progn
      (entmake (list '(0 . "BLOCK") '(100 . "AcDbEntity") '(8 . "0") '(100 . "AcDbBlockBegin") '(2 . "버팀보(Strut)_2H300x300") '(70 . 0) '(10 0.0 0.0 0.0)))
      (entmake (list '(0 . "HATCH") '(100 . "AcDbEntity") '(8 . "_버팀보(Strut)") '(100 . "AcDbHatch") '(10 0.0 0.0 0.0) '(210 0.0 0.0 1.0) '(2 . "SOLID") '(70 . 1) '(71 . 0) '(91 . 1) '(92 . 1) '(93 . 4) '(72 . 1) '(10 600.0 0.0 0.0) '(11 600.0 300.0 0.0) '(72 . 1) '(10 600.0 300.0 0.0) '(11 900.0 300.0 0.0) '(72 . 1) '(10 900.0 300.0 0.0) '(11 900.0 0.0 0.0) '(72 . 1) '(10 900.0 0.0 0.0) '(11 600.0 0.0 0.0) '(97 . 0) '(75 . 1) '(76 . 1) '(98 . 1) '(10 600.0 0.0 0.0) '(450 . 0) '(451 . 0) '(460 . 0.0) '(461 . 0.0) '(452 . 0) '(462 . 0.0) '(453 . 2) '(463 . 0.0) '(63 . 5) '(421 . 255) '(463 . 1.0) '(63 . 2) '(421 . 16776960) '(470 . "LINEAR")))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "_버팀보(Strut)") '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1) '(10 600.0 0.0) '(10 600.0 300.0) '(10 900.0 300.0) '(10 900.0 0.0)))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "_버팀보(Strut)") '(6 . "ByBlock") '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1) '(10 0.0 300.0) '(10 900.0 300.0) '(10 900.0 0.0) '(10 0.0 0.0)))
      (entmake (list '(0 . "HATCH") '(100 . "AcDbEntity") '(8 . "_버팀보(Strut)") '(100 . "AcDbHatch") '(10 0.0 0.0 0.0) '(210 0.0 0.0 1.0) '(2 . "SOLID") '(70 . 1) '(71 . 0) '(91 . 1) '(92 . 1) '(93 . 4) '(72 . 1) '(10 0.0 0.0 0.0) '(11 0.0 300.0 0.0) '(72 . 1) '(10 0.0 300.0 0.0) '(11 300.0 300.0 0.0) '(72 . 1) '(10 300.0 300.0 0.0) '(11 300.0 0.0 0.0) '(72 . 1) '(10 300.0 0.0 0.0) '(11 0.0 0.0 0.0) '(97 . 0) '(75 . 1) '(76 . 1) '(98 . 1) '(10 0.0 0.0 0.0) '(450 . 0) '(451 . 0) '(460 . 0.0) '(461 . 0.0) '(452 . 0) '(462 . 0.0) '(453 . 2) '(463 . 0.0) '(63 . 5) '(421 . 255) '(463 . 1.0) '(63 . 2) '(421 . 16776960) '(470 . "LINEAR")))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "_버팀보(Strut)") '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1) '(10 0.0 0.0) '(10 0.0 300.0) '(10 300.0 300.0) '(10 300.0 0.0)))
      (entmake '((0 . "ENDBLK") (100 . "AcDbEntity") (8 . "0") (100 . "AcDbBlockEnd")))
    )
  )
)

;; --- [2] 세그먼트 및 SUB-ZONES 데이터 추출 보조 함수 ---
(defun drg-extract-wall-width-from-zone (z-data seg-data / w-type vals h-spec)
  (setq w-type (if (assoc 'WALL-TYPE z-data) (cdr (assoc 'WALL-TYPE z-data))
                 (if (assoc 'WALL-TYPE seg-data) (cdr (assoc 'WALL-TYPE seg-data)) "HPILE")))
  (setq vals (cdr (assoc 'WALL-CUSTOM z-data)))
  (if (null vals) (setq vals (cdr (assoc 'WALL-CUSTOM seg-data))))
  (if (and vals (listp vals) (>= (length vals) 2))
    (float (nth 1 vals))
    (progn
      (setq h-spec (if (assoc 'WALL-SPEC z-data) (cdr (assoc 'WALL-SPEC z-data))
                     (if (assoc 'WALL-SPEC seg-data) (cdr (assoc 'WALL-SPEC seg-data)) "H 300x300x10/15")))
      (cond
        ((vl-string-search "250" h-spec) 250.0)
        ((or (vl-string-search "298" h-spec) (vl-string-search "201" h-spec)) 201.0)
        ((vl-string-search "350" h-spec) 350.0)
        ((vl-string-search "400" h-spec) 400.0)
        (t 300.0)
      )
    )
  )
)

(defun drg-extract-embed-from-zone (z-data seg-data / w-type emb-val)
  (setq w-type (if (assoc 'WALL-TYPE z-data) (cdr (assoc 'WALL-TYPE z-data))
                 (if (assoc 'WALL-TYPE seg-data) (cdr (assoc 'WALL-TYPE seg-data)) "HPILE")))
  (setq emb-val (if (assoc 'EMBED-DEPTH z-data) (cdr (assoc 'EMBED-DEPTH z-data)) nil))
  (if (null emb-val)
    (if (and (= w-type "CIP") (assoc 'CIP-EMBED-DEPTH z-data))
      (setq emb-val (atof (cdr (assoc 'CIP-EMBED-DEPTH z-data))))
    )
  )
  (if (null emb-val)
    (setq emb-val (if (assoc 'EMBED-DEPTH seg-data) (cdr (assoc 'EMBED-DEPTH seg-data)) 3.0))
  )
  (if (= (type emb-val) 'STR) (setq emb-val (atof emb-val)))
  (float emb-val)
)

(defun drg-extract-ctc-from-zone (z-data seg-data / ctc-val)
  (setq ctc-val (if (assoc 'CTC z-data) (cdr (assoc 'CTC z-data))
                  (if (assoc 'CTC seg-data) (cdr (assoc 'CTC seg-data)) 2.0)))
  (if (= (type ctc-val) 'STR) (setq ctc-val (atof ctc-val)))
  (if (<= ctc-val 0.0) (setq ctc-val 2.0))
  (float ctc-val)
)

;; TSP_1C_Globals_Utils.lsp 의 tsp-get-rock-start-depth 와 동일한 암반 시작 심도(m) 탐색 함수
(defun drg-get-rock-depth-from-soil (soil-layers sc-enable sc-chk sc-layer / prev-d r-dep found lyr l-name l-depth l-type)
  (setq prev-d 0.0 r-dep nil found nil)
  (if (null sc-enable) (setq sc-enable "1"))
  (if (and (= sc-enable "1") soil-layers)
    (foreach lyr soil-layers
      (if (not found)
        (progn
          (setq l-name (if (car lyr) (vl-princ-to-string (car lyr)) ""))
          (setq l-depth (if (= (type (cadr lyr)) 'STR) (atof (cadr lyr)) (float (cadr lyr))))
          (setq l-type (if (> (length lyr) 5) (vl-princ-to-string (nth 5 lyr)) ""))
          (if (and sc-chk (= sc-chk "1") sc-layer (/= sc-layer ""))
            (if (= (strcase l-name) (strcase sc-layer))
              (progn (setq r-dep prev-d found T))
            )
            (if (or (wcmatch (strcase l-type) "*풍화암*,*연암*,*경암*,*WR*,*SR*,*HR*,*암*")
                    (wcmatch (strcase l-name) "*풍화암*,*연암*,*경암*,*WR*,*SR*,*HR*,*암*"))
              (progn (setq r-dep prev-d found T))
            )
          )
          (setq prev-d l-depth)
        )
      )
    )
  )
  r-dep
)

;; 도면의 MEASUREMENT 변수에 따른 해치 스케일 보정 계수 반환 (TSP_1A_Globals_Utils.lsp 호환)
(defun drg-get-hatch-scale-factor ( / )
  (if (boundp 'get-hatch-scale-factor)
    (get-hatch-scale-factor)
    (if (= (getvar "MEASUREMENT") 0) 25.4 1.0)
  )
)

(defun drg-parse-wale-height (w-spec / )
  (if (null w-spec)
    300.0
    (cond
      ((vl-string-search "250" w-spec) 250.0)
      ((or (vl-string-search "298" w-spec) (vl-string-search "201" w-spec)) 201.0)
      ((vl-string-search "350" w-spec) 350.0)
      ((vl-string-search "400" w-spec) 400.0)
      (t 300.0)
    )
  )
)

;; TSP 모서리 좌표(corner-x-list)를 직접 받아 세그먼트 및 SUB-ZONES 스팬을 생성하는 함수
;; (지하수위, 숏크리트 설정, 차수그라우팅 규격/간격/모드 데이터 포함)
(defun drg-build-tsp-elevation-spans (seg-list corner-x-list prof-start-x /
                                      spans cum-dist idx num-segs seg seg-len c1-x c2-x scale-f sub-spans sp
                                      a-mm b-mm sz s-x e-x emb-m pw-mm ctc-m w-type wale-sp sup-lst max-d soil-lst rock-emb c-brace
                                      w-chk w-dep sc-en sc-chk sc-lyr gr-idx gr-sp-idx gr-sp-cus cip-dia cip-mode cip-ovl)
  (setq spans nil cum-dist 0.0 idx 0 num-segs (length seg-list))
  (foreach seg seg-list
    (setq seg-len (float (cdr (assoc 'LENGTH seg))))
    (if (and corner-x-list (> (length corner-x-list) (1+ idx)))
      (progn
        (setq c1-x (if (listp (nth idx corner-x-list)) (cadr (nth idx corner-x-list)) (float (nth idx corner-x-list))))
        (setq c2-x (if (listp (nth (1+ idx) corner-x-list)) (cadr (nth (1+ idx) corner-x-list)) (float (nth (1+ idx) corner-x-list))))
      )
      (progn
        (setq c1-x (+ prof-start-x cum-dist))
        (setq c2-x (+ c1-x seg-len))
      )
    )
    (setq scale-f (if (> seg-len 1e-3) (/ (- c2-x c1-x) seg-len) 1.0))

    (if (boundp 'tsp-resolve-wall-spans)
      (setq sub-spans (tsp-resolve-wall-spans seg))
      (setq sub-spans (list (list 0.0 seg-len seg)))
    )

    (foreach sp sub-spans
      (setq a-mm (float (nth 0 sp)) b-mm (float (nth 1 sp)) sz (nth 2 sp))
      (setq s-x (+ c1-x (* a-mm scale-f)))
      (setq e-x (+ c1-x (* b-mm scale-f)))
      (setq emb-m (drg-extract-embed-from-zone sz seg))
      (setq pw-mm (drg-extract-wall-width-from-zone sz seg))
      (setq ctc-m (drg-extract-ctc-from-zone sz seg))
      (setq w-type (if (assoc 'WALL-TYPE sz) (cdr (assoc 'WALL-TYPE sz)) (cdr (assoc 'WALL-TYPE seg))))
      (setq wale-sp (if (assoc 'WALE-SPEC sz) (cdr (assoc 'WALE-SPEC sz)) (cdr (assoc 'WALE-SPEC seg))))
      (setq sup-lst (if (assoc 'SUPPORT-LIST sz) (cdr (assoc 'SUPPORT-LIST sz)) (cdr (assoc 'SUPPORT-LIST seg))))
      (setq max-d (if (assoc 'MAX-DEPTH sz) (cdr (assoc 'MAX-DEPTH sz)) (cdr (assoc 'MAX-DEPTH seg))))
      (if (null max-d)
        (setq max-d (if (assoc 'CIP-MAX-DEPTH sz) (atof (cdr (assoc 'CIP-MAX-DEPTH sz)))
                      (if (assoc 'CIP-MAX-DEPTH seg) (atof (cdr (assoc 'CIP-MAX-DEPTH seg))) 10.0)))
      )
      (if (= (type max-d) 'STR) (setq max-d (atof max-d)))
      (setq soil-lst (if (assoc 'SOIL-DATA sz) (cdr (assoc 'SOIL-DATA sz)) (cdr (assoc 'SOIL-DATA seg))))
      (setq rock-emb (if (assoc 'ROCK-EMBED sz) (cdr (assoc 'ROCK-EMBED sz)) (cdr (assoc 'ROCK-EMBED seg))))
      (setq c-brace (cdr (assoc 'CORNER-BRACE seg)))

      ;; 추가 추출: 지하수위, 숏크리트 옵션, 그라우팅 및 CIP 배열 제원
      (setq w-chk (if (assoc 'WATER-CHK sz) (cdr (assoc 'WATER-CHK sz)) (if (assoc 'WATER-CHK seg) (cdr (assoc 'WATER-CHK seg)) "0")))
      (setq w-dep (if (assoc 'WATER-DEPTH sz) (cdr (assoc 'WATER-DEPTH sz)) (if (assoc 'WATER-DEPTH seg) (cdr (assoc 'WATER-DEPTH seg)) 0.0)))
      (if (= (type w-dep) 'STR) (setq w-dep (atof w-dep)))
      (setq sc-en (if (assoc 'SHOTCRETE-ENABLE sz) (cdr (assoc 'SHOTCRETE-ENABLE sz)) (if (assoc 'SHOTCRETE-ENABLE seg) (cdr (assoc 'SHOTCRETE-ENABLE seg)) "1")))
      (setq sc-chk (if (assoc 'SHOTCRETE-CHK sz) (cdr (assoc 'SHOTCRETE-CHK sz)) (if (assoc 'SHOTCRETE-CHK seg) (cdr (assoc 'SHOTCRETE-CHK seg)) "0")))
      (setq sc-lyr (if (assoc 'SHOTCRETE-LAYER sz) (cdr (assoc 'SHOTCRETE-LAYER sz)) (if (assoc 'SHOTCRETE-LAYER seg) (cdr (assoc 'SHOTCRETE-LAYER seg)) "")))
      (setq gr-idx (if (assoc 'CIP-GROUTING-IDX sz) (cdr (assoc 'CIP-GROUTING-IDX sz)) (if (assoc 'CIP-GROUTING-IDX seg) (cdr (assoc 'CIP-GROUTING-IDX seg)) "0")))
      (setq gr-sp-idx (if (assoc 'CIP-GROUTING-SPACE-IDX sz) (cdr (assoc 'CIP-GROUTING-SPACE-IDX sz)) (if (assoc 'CIP-GROUTING-SPACE-IDX seg) (cdr (assoc 'CIP-GROUTING-SPACE-IDX seg)) "0")))
      (setq gr-sp-cus (if (assoc 'CIP-GROUTING-SPACE-CUSTOM sz) (cdr (assoc 'CIP-GROUTING-SPACE-CUSTOM sz)) (if (assoc 'CIP-GROUTING-SPACE-CUSTOM seg) (cdr (assoc 'CIP-GROUTING-SPACE-CUSTOM seg)) "")))
      (setq cip-dia (if (assoc 'CIP-DIA sz) (cdr (assoc 'CIP-DIA sz)) (if (assoc 'CIP-DIA seg) (cdr (assoc 'CIP-DIA seg)) "500")))
      (setq cip-mode (if (assoc 'CIP-MODE-IDX sz) (cdr (assoc 'CIP-MODE-IDX sz)) (if (assoc 'CIP-MODE-IDX seg) (cdr (assoc 'CIP-MODE-IDX seg)) "0")))
      (setq cip-ovl (if (assoc 'CIP-OVERLAP sz) (cdr (assoc 'CIP-OVERLAP sz)) (if (assoc 'CIP-OVERLAP seg) (cdr (assoc 'CIP-OVERLAP seg)) "0")))

      ;; 스팬 구조:
      ;; 0:시작X 1:끝X 2:근입(mm) 3:말뚝폭(mm) 4:CTC(mm) 5:세그번호 6:벽체공법 7:굴착깊이(m)
      ;; 8:띠장규격 9:지보재리스트 10:지층데이터 11:암반정착(mm) 12:사보강재 13:세그시작X 14:세그끝X 15:스케일비율
      ;; 16:수위체크 17:수위심도(m) 18:숏크리트활성 19:숏크리트지정체크 20:숏크리트지정층
      ;; 21:그라우팅규격idx 22:그라우팅간격idx 23:그라우팅간격커스텀 24:CIP직경 25:CIP모드idx 26:CIP겹침량
      (setq spans
        (append spans
          (list
            (list s-x e-x (* emb-m 1000.0) pw-mm (* ctc-m 1000.0) (1+ idx) w-type (float max-d) wale-sp sup-lst soil-lst rock-emb c-brace c1-x c2-x scale-f
                  w-chk (float w-dep) sc-en sc-chk sc-lyr gr-idx gr-sp-idx gr-sp-cus cip-dia cip-mode cip-ovl)
          )
        )
      )
    )
    (setq cum-dist (+ cum-dist (- c2-x c1-x)))
    (setq idx (1+ idx))
  )
  (setq *drg-last-tsp-spans* spans)
  spans
)

;; --- [3] 다각형 해치 생성 보조 함수 (단면도 해치 제원 완벽 호환) ---
(defun drg-make-closed-poly-and-hatch (pts lyr col pat scl ang / ent-list poly-ent doc mspace hatch-obj sa)
  (if (>= (length pts) 3)
    (progn
      (setq ent-list (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                           (cons 8 lyr) (cons 62 col) (cons 90 (length pts)) '(70 . 1)))
      (foreach p pts
        (setq ent-list (append ent-list (list (cons 10 (list (car p) (cadr p))))))
      )
      (setq poly-ent (entmakex ent-list))
      (if poly-ent
        (progn
          (setq doc (vla-get-activedocument (vlax-get-acad-object)))
          (setq mspace (if (= (getvar "CVPORT") 1) (vla-get-paperspace doc) (vla-get-modelspace doc)))
          (setq hatch-obj (vl-catch-all-apply 'vla-addhatch (list mspace 1 pat :vlax-true)))
          (if (not (vl-catch-all-error-p hatch-obj))
            (progn
              (vla-put-patternscale hatch-obj (float scl))
              (vla-put-patternangle hatch-obj (float ang))
              (vla-put-layer hatch-obj lyr)
              (vla-put-color hatch-obj col)
              (setq sa (vlax-make-safearray 9 (cons 0 0)))
              (vlax-safearray-fill sa (list (vlax-ename->vla-object poly-ent)))
              (vl-catch-all-apply 'vla-appendouterloop (list hatch-obj sa))
              (vl-catch-all-apply 'vla-evaluate (list hatch-obj))
              (list poly-ent (vlax-vla-object->ename hatch-obj))
            )
            (list poly-ent)
          )
        )
      )
    )
  )
)

;; 구간 내 폴리선(지반선/굴착바닥고) 샘플링 점 생성 (실제 지반선 _GRND 경사 평행 추종 지원)
(defun drg-sample-band-polygon (ent-grnd ent-excav x1 x2 mode const-top-y const-bot-y exc-offset /
                                xs v-grnd v-exc k step-mm clean-xs top-pts bot-pts x y-g y-e y-t y-b)
  (setq xs (list x1 x2))
  ;; 구간 내부에 있는 지반선(_GRND) 및 굴착바닥고(_굴착바닥고)의 실제 절점(Vertex) X좌표를 모두 포함하여 경사/단차 완벽 추종
  (setq v-grnd (get-pline-vertices-x ent-grnd))
  (setq v-exc  (get-pline-vertices-x ent-excav))
  (foreach vx (append v-grnd v-exc)
    (if (and (> vx (+ x1 1.0)) (< vx (- x2 1.0)))
      (setq xs (append xs (list vx)))
    )
  )
  (setq step-mm (if (<= (- x2 x1) 1000.0) 200.0 1000.0))
  (setq k x1)
  (while (< (+ k step-mm) x2)
    (setq k (+ k step-mm))
    (setq xs (append xs (list k)))
  )
  (setq xs (vl-sort xs '<))
  (setq clean-xs nil)
  (foreach x xs
    (if (or (null clean-xs) (> (abs (- x (last clean-xs))) 1.0))
      (setq clean-xs (append clean-xs (list x)))
    )
  )
  (setq top-pts nil bot-pts nil)
  (foreach x clean-xs
    (setq y-g (get-y-on-pline ent-grnd x))
    (setq y-e (get-y-on-pline ent-excav x))
    (if (and y-g y-e)
      (progn
        (cond
          ((= mode "GRND_TO_EXC")
           (setq y-t y-g y-b y-e)
          )
          ;; [경사 지반선 평행 추종] 지반선(y-g) ~ 지반선에서 const-bot-y(mm)만큼 평행하게 내린 깊이(최하단 y-e 제한)
          ((= mode "GRND_TO_REL_DEPTH")
           (setq y-t y-g)
           (setq y-b (min y-g (max y-e (- y-g const-bot-y))))
          )
          ;; [경사 지반선 평행 추종] 지반선에서 const-top-y(mm)만큼 평행하게 내린 깊이 ~ 굴착바닥고(y-e)
          ((= mode "REL_DEPTH_TO_EXC")
           (setq y-t (min y-g (max y-e (- y-g const-top-y))))
           (setq y-b y-e)
          )
          ((= mode "GRND_TO_Y")
           (setq y-t y-g)
           (setq y-b (min y-g (max y-e const-bot-y)))
          )
          ((= mode "Y_TO_EXC")
           (setq y-t (min y-g (max y-e const-top-y)))
           (setq y-b y-e)
          )
          ((= mode "GRND_TO_PILE")
           (setq y-t y-g)
           (setq y-b (- y-e exc-offset))
          )
        )
        (setq top-pts (append top-pts (list (list x y-t))))
        (setq bot-pts (append bot-pts (list (list x y-b))))
      )
    )
  )
  (if (vl-some '(lambda (pair) (> (- (cadr (car pair)) (cadr (cadr pair))) 10.0))
               (mapcar 'list top-pts bot-pts))
    (append top-pts (reverse bot-pts))
    nil
  )
)

;; 그라우팅 개별 폭 구간([x - D/2, x + D/2]) 중 서로 맞닿거나 겹치는 구간들을 단일 해치 구간으로 병합하는 함수
;; (D200 연속 시공이나 D600 중첩 시공 시 불필요한 내부 외곽선을 그리지 않고 하나의 해치로 통합하며,
;;  CIP 사이사이에 이격 배치된 D200 자동배열은 각 공의 폭 D만큼 개별 해치로 유지함)
(defun drg-merge-grout-intervals (raw-intervals min-x max-x / clipped sorted merged cur nxt c-s c-e)
  (setq clipped nil)
  (foreach iv raw-intervals
    (setq c-s (max min-x (float (car iv))))
    (setq c-e (min max-x (float (cadr iv))))
    (if (> (- c-e c-s) 1.0)
      (setq clipped (append clipped (list (list c-s c-e))))
    )
  )
  (if clipped
    (progn
      (setq sorted (vl-sort clipped '(lambda (a b) (< (car a) (car b)))))
      (setq merged nil cur (car sorted))
      (foreach nxt (cdr sorted)
        ;; 인접한 두 그라우팅 구간이 맞닿거나 겹치면(오차 5mm 허용) 하나로 병합
        (if (<= (car nxt) (+ (cadr cur) 5.0))
          (setq cur (list (car cur) (max (cadr cur) (cadr nxt))))
          (progn
            (setq merged (append merged (list cur)))
            (setq cur nxt)
          )
        )
      )
      (if cur (setq merged (append merged (list cur))))
      merged
    )
    nil
  )
)

;; 직사각형(띠장) 생성 함수
(defun drg-draw-wale-rect (x1 x2 y1 y2 lyr col / p1 p2 p3 p4)
  (setq p1 (list x1 (min y1 y2))
        p2 (list x2 (min y1 y2))
        p3 (list x2 (max y1 y2))
        p4 (list x1 (max y1 y2)))
  (entmakex (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                  (cons 8 lyr) (cons 62 col) '(90 . 4) '(70 . 1)
                  (cons 10 p1) (cons 10 p2) (cons 10 p3) (cons 10 p4)))
)

;; 지보재 블록 중심 기준 삽입 함수 (Base Point 보정)
(defun drg-insert-support-block-centered (blk-name lyr cx cy / dx dy ins-pt)
  (cond
    ((= blk-name "앵커(Anchor)")
     (setq dx 135.0 dy 325.0)
    )
    ((wcmatch blk-name "*_2H*")
     (setq dx 450.0 dy 150.0)
    )
    (t
     (setq dx 150.0 dy 150.0)
    )
  )
  (setq ins-pt (list (- cx dx) (- cy dy) 0.0))
  (entmakex (list '(0 . "INSERT") '(100 . "AcDbEntity") (cons 8 lyr) '(62 . 256)
                  '(100 . "AcDbBlockReference") (cons 2 blk-name) (cons 10 ins-pt)
                  '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) '(50 . 0.0)))
)

;; 평면도에서 투영할 지보재 위치(_mpoint 수선 투영 + 앵커선 경계선 교차점 투영) 추출 함수
(defun drg-collect-projected-supports (ss-plan ent-bound is-closed is-forward base-start-dist total-bound-dist prof-start-x /
                                       pt-to-x mpoints strut-xs corner-xs anchor-xs vla-bnd bnd-elev k e ed etyp elay pt xval
                                       vla-e e-elev int-pts idx i-pt)
  (defun pt-to-x (pt / cpt cur-dist chain)
    (setq cpt (vlax-curve-getClosestPointTo ent-bound (list (car pt) (cadr pt) 0.0)))
    (if cpt
      (progn
        (setq cur-dist (vlax-curve-getDistAtPoint ent-bound cpt))
        (if is-closed
          (progn
            (if is-forward
              (setq chain (- cur-dist base-start-dist))
              (setq chain (- base-start-dist cur-dist))
            )
            (if (< (abs chain) 0.01) (setq chain 0.0))
            (if (< chain -0.01) (setq chain (+ total-bound-dist chain)))
          )
          (progn
            (setq chain cur-dist)
            (if (not is-forward) (setq chain (- total-bound-dist chain)))
          )
        )
        (+ prof-start-x chain)
      )
      nil
    )
  )

  (setq mpoints nil strut-xs nil corner-xs nil anchor-xs nil)
  (setq vla-bnd (vlax-ename->vla-object ent-bound))
  (setq bnd-elev (if (= (cdr (assoc 0 (entget ent-bound))) "LWPOLYLINE") (vlax-get-property vla-bnd 'Elevation) 0.0))

  (if ss-plan
    (progn
      (setq k 0)
      (while (< k (sslength ss-plan))
        (setq e (ssname ss-plan k) ed (entget e) etyp (cdr (assoc 0 ed)) elay (strcase (cdr (assoc 8 ed))))
        (cond
          ((and (= etyp "POINT") (wcmatch elay "*MPOINT*"))
           (setq pt (cdr (assoc 10 ed)))
           (setq mpoints (append mpoints (list pt)))
          )
          ((and (member etyp '("LINE" "LWPOLYLINE"))
                (wcmatch elay "*앵커*")
                (not (and (= etyp "LWPOLYLINE") (assoc 70 ed) (= (logand (cdr (assoc 70 ed)) 1) 1))))
           (setq vla-e (vlax-ename->vla-object e))
           (if (= etyp "LWPOLYLINE")
             (progn
               (setq e-elev (vlax-get-property vla-e 'Elevation))
               (vlax-put-property vla-e 'Elevation bnd-elev)
               (setq int-pts (vl-catch-all-apply 'vlax-invoke (list vla-bnd 'IntersectWith vla-e 0)))
               (vlax-put-property vla-e 'Elevation e-elev)
             )
             (setq int-pts (vl-catch-all-apply 'vlax-invoke (list vla-bnd 'IntersectWith vla-e 0)))
           )
           (if (and (not (vl-catch-all-error-p int-pts)) int-pts)
             (progn
               (setq idx 0)
               (while (< idx (length int-pts))
                 (setq i-pt (list (nth idx int-pts) (nth (1+ idx) int-pts) 0.0))
                 (setq xval (pt-to-x i-pt))
                 (if (and xval (not (vl-some '(lambda (x) (< (abs (- x xval)) 50.0)) anchor-xs)))
                   (setq anchor-xs (append anchor-xs (list xval)))
                 )
                 (setq idx (+ idx 3))
               )
             )
           )
          )
        )
        (setq k (1+ k))
      )

      (foreach mpt mpoints
        (setq xval (pt-to-x mpt))
        (if xval
          (if (ssget "C" (list (- (car mpt) 400.0) (- (cadr mpt) 400.0))
                         (list (+ (car mpt) 400.0) (+ (cadr mpt) 400.0))
                         '((8 . "_사보강재*")))
            (if (not (vl-some '(lambda (x) (< (abs (- x xval)) 50.0)) corner-xs))
              (setq corner-xs (append corner-xs (list xval)))
            )
            (if (not (vl-some '(lambda (x) (< (abs (- x xval)) 50.0)) strut-xs))
              (setq strut-xs (append strut-xs (list xval)))
            )
          )
        )
      )
    )
  )
  (list (vl-sort strut-xs '<) (vl-sort corner-xs '<) (vl-sort anchor-xs '<))
)

;; CORNER-BRACE 데이터에서 단별 사보강재 목록을 추출하는 보조 함수 (DRG 단독 실행 시에도 호환)
(defun drg-get-corner-brace-stages (c-brace seg-obj / sup-lst stages idx s-it)
  (cond
    ((or (null c-brace) (/= (car c-brace) "Y")) '())
    ((and (> (length c-brace) 12) (= (nth 11 c-brace) "MULTI") (listp (nth 12 c-brace)))
     (nth 12 c-brace)
    )
    ((and (cadr c-brace) (listp (cadr c-brace)) (> (length (cadr c-brace)) 0))
     (setq sup-lst (if seg-obj (cdr (assoc 'SUPPORT-LIST seg-obj)) nil))
     (if sup-lst
       (progn
         (setq stages '() idx 1)
         (foreach s-it sup-lst
           (if (and (listp s-it) (> (length s-it) 3))
             (progn
               (setq stages (append stages (list (list (strcat "Corner_" (itoa idx)) (float (cadddr s-it)) (cadr c-brace)))))
               (setq idx (1+ idx))
             )
           )
         )
         stages
       )
       (list (list "Corner_1" 1.5 (cadr c-brace)))
     )
    )
    (t '())
  )
)

;; [맞은편 버팀보 역추적 보조 함수]
(defun drg-find-opposite-strut-targets (src-seg-idx s-item seg-list seg-span-map bnd-orient /
                                        src-seg v-s v-e v-a s-len orient norm-ang brace-opt ang-off-deg ray-ang
                                        manual-pts test-pts s-ctc-mm d-cur pt pt-flat p-ray
                                        best-tid best-d best-hit-x t-idx t-seg tv-s tv-e tv-a t-len
                                        sp1 sp2 int-pt d-ray dx dy u-proj t-sp t-c1-x t-scale hit-x
                                        hit-map existing-entry new-xs)
  (setq src-seg (nth src-seg-idx seg-list))
  (setq v-s (cdr (assoc 'V-START src-seg))
        v-e (cdr (assoc 'V-END src-seg))
        v-a (cdr (assoc 'ANGLE src-seg))
        s-len (float (cdr (assoc 'LENGTH src-seg))))
  (setq orient (if bnd-orient bnd-orient -1))
  (setq norm-ang (+ v-a (* orient (/ pi 2.0))))
  (setq brace-opt (if (> (length s-item) 11) (nth 11 s-item) nil))
  (setq ang-off-deg (if (and (listp brace-opt) (> (length brace-opt) 14) (nth 14 brace-opt))
                      (atof (vl-princ-to-string (nth 14 brace-opt)))
                      0.0))
  (setq ray-ang (+ norm-ang (* ang-off-deg (/ pi 180.0))))

  (setq manual-pts (if (> (length s-item) 8) (nth 8 s-item) nil))
  (if (and manual-pts (listp manual-pts) (> (length manual-pts) 0))
    (setq test-pts manual-pts)
    (progn
      (setq s-ctc-mm (* (if (and (> (length s-item) 10) (nth 10 s_item)) (atof (vl-princ-to-string (nth 10 s-item))) 5.0) 1000.0))
      (if (<= s-ctc-mm 100.0) (setq s-ctc-mm 5000.0))
      (setq test-pts nil d-cur (/ s-ctc-mm 2.0))
      (while (<= d-cur (- s-len 200.0))
        (setq test-pts (append test-pts (list (polar v-s v-a d-cur))))
        (setq d-cur (+ d-cur s-ctc-mm))
      )
      (if (null test-pts)
        (setq test-pts (list (polar v-s v-a (/ s-len 2.0))))
      )
    )
  )

  (setq hit-map nil)
  (foreach pt test-pts
    (if (and pt (listp pt) (>= (length pt) 2))
      (progn
        (setq pt-flat (list (car pt) (cadr pt) 0.0))
        (setq best-tid nil best-d 1e99 best-hit-x nil)
        (foreach try-ang (list ray-ang (+ ray-ang pi))
          (if (null best-tid)
            (progn
              (setq p-ray (polar pt-flat try-ang 1e6))
              (setq t-idx 0)
              (while (< t-idx (length seg-list))
                (if (/= t-idx src-seg-idx)
                  (progn
                    (setq t-seg (nth t-idx seg-list))
                    (setq tv-s (cdr (assoc 'V-START t-seg))
                          tv-e (cdr (assoc 'V-END t-seg))
                          tv-a (cdr (assoc 'ANGLE t-seg))
                          t-len (float (cdr (assoc 'LENGTH t-seg))))
                    (if (and tv-s tv-e (> t-len 1.0))
                      (progn
                        (setq sp1 (list (car tv-s) (cadr tv-s) 0.0))
                        (setq sp2 (list (car tv-e) (cadr tv-e) 0.0))
                        (setq int-pt (inters pt-flat p-ray sp1 sp2 T))
                        (if int-pt
                          (progn
                            (setq d-ray (distance pt-flat int-pt))
                            (if (and (> d-ray 100.0) (< d-ray best-d))
                              (progn
                                (setq dx (- (car int-pt) (car sp1))
                                      dy (- (cadr int-pt) (cadr sp1)))
                                (setq u-proj (+ (* dx (cos tv-a)) (* dy (sin tv-a))))
                                (setq u-proj (max 0.0 (min t-len u-proj)))
                                (setq t-sp (assoc (1+ t-idx) seg-span-map))
                                (if t-sp
                                  (progn
                                    (setq t-c1-x (nth 1 t-sp)
                                          t-scale (nth 3 t-sp))
                                    (setq hit-x (+ t-c1-x (* u-proj t-scale)))
                                    (setq best-d d-ray
                                          best-tid (1+ t-idx)
                                          best-hit-x hit-x)
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
                (setq t-idx (1+ t-idx))
              )
            )
          )
        )
        (if (and best-tid best-hit-x)
          (progn
            (setq existing-entry (assoc best-tid hit-map))
            (if existing-entry
              (progn
                (setq new-xs (cadr existing-entry))
                (if (not (vl-some '(lambda (x) (< (abs (- x best-hit-x)) 50.0)) new-xs))
                  (setq new-xs (append new-xs (list best-hit-x)))
                )
                (setq hit-map (subst (list best-tid new-xs) existing-entry hit-map))
              )
              (setq hit-map (append hit-map (list (list best-tid (list best-hit-x)))))
            )
          )
        )
      )
    )
  )
  hit-map
)

;; --- [3-B] 평면도 <-> 전개도 단면 위치 양방향 변환, 관통단면 역산 및 '_단면표시' 기호 작도 함수 ---

;; '맑은 고딕' 텍스트 스타일 보장 함수
(defun drg-ensure-section-text-style ( / )
  (if (not (tblsearch "STYLE" "맑은 고딕"))
    (entmake
      (list
        '(0 . "STYLE")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbTextStyleTableRecord")
        '(2 . "맑은 고딕")
        '(70 . 0)
        '(40 . 0.0)
        '(41 . 1.0)
        '(50 . 0.0)
        '(71 . 0)
        '(42 . 1000.0)
        '(3 . "malgun.ttf")
        '(4 . "")
      )
    )
  )
)

;; LIST 제원(상수폭 66.6667 LWPOLYLINE + 반쪽 SOLID 화살촉 + 높이 1000.0 TEXT)과 동일한 단일 단면기호 생성 함수
;;   - p-cross  : 절단 축선과 시선 방향 화살표 축선이 교차하는 기준점 (2D/3D 좌표)
;;   - cut-dir  : 절단선 안쪽에서 바깥쪽(기호 끝단)을 향하는 각도 (rad)
;;   - view-dir : 시선(화살표)이 가리키는 방향 각도 (rad)
;;   - label-str: 표기할 단면 문자 (예: "A" 또는 "A (Seg-1)")
(defun drg-draw-single-section-symbol (p-cross cut-dir view-dir label-str /
                                       lyr col p0 p1 p2 p3 p4 p5 p6 p7 p-mid-base p-txt
                                       e-pl e-sol e-txt)
  (setq lyr "_단면표시" col 10)
  (if (not (tblsearch "LAYER" lyr))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                   (cons 2 lyr) '(70 . 0) (cons 62 col) '(6 . "Continuous")))
  )
  (drg-ensure-section-text-style)

  (setq p0 (list (car p-cross) (cadr p-cross) 0.0))
  ;; LIST 역산 치수:
  ;;   - 절단선 내측 시작점(p1): 교차점(p0)에서 cut-dir 반대방향으로 1523.8095
  ;;   - 절단선 외측 끝점(p2)  : 교차점(p0)에서 cut-dir 방향으로 380.9524 (총 절단선 조각 길이 = 1904.7619)
  ;;   - 시선 후방 끝점(p4)    : 교차점(p0)에서 view-dir 반대방향으로 380.9524
  ;;   - 화살촉 끝점(p5)       : 교차점(p0)에서 view-dir 방향으로 2095.2381
  ;;   - 화살촉 길이 = 761.9047 (중심점 거리 = 1333.3334), 화살촉 반폭 = 438.11465
  (setq p1 (polar p0 (+ cut-dir pi) 1523.8095))
  (setq p2 (polar p0 cut-dir 380.9524))
  (setq p3 p0)
  (setq p4 (polar p0 (+ view-dir pi) 380.9524))
  (setq p5 (polar p0 view-dir 2095.2381))
  (setq p-mid-base (polar p0 view-dir 1333.3334))
  ;; p6: 내측 날개 끝점, p7: 외측(cut-dir 방향) 날개 끝점 (SOLID 채움 대상)
  (setq p6 (polar p-mid-base (+ cut-dir pi) 438.11465))
  (setq p7 (polar p-mid-base cut-dir 438.11465))
  ;; 텍스트 위치: 화살촉 끝점(p5)에서 view-dir 방향으로 841.3772 이동 후 문자 길이만큼 추가 보정
  (setq p-txt (polar p5 view-dir (+ 841.3772 (* (max 0 (1- (strlen label-str))) 320.0))))

  ;; 1. 8점 LWPOLYLINE (상수폭 66.6667)
  (setq e-pl
    (entmakex
      (list
        '(0 . "LWPOLYLINE")
        '(100 . "AcDbEntity")
        (cons 8 lyr)
        (cons 62 col)
        '(100 . "AcDbPolyline")
        '(90 . 8)
        '(70 . 0)
        '(43 . 66.6667)
        (cons 10 (list (car p1) (cadr p1)))
        (cons 10 (list (car p2) (cadr p2)))
        (cons 10 (list (car p3) (cadr p3)))
        (cons 10 (list (car p4) (cadr p4)))
        (cons 10 (list (car p5) (cadr p5)))
        (cons 10 (list (car p6) (cadr p6)))
        (cons 10 (list (car p7) (cadr p7)))
        (cons 10 (list (car p5) (cadr p5)))
      )
    )
  )

  ;; 2. 외측 절반 SOLID 화살촉 채움 (시작점 p5, 날개점 p7, 밑변중심 p-mid-base)
  (setq e-sol
    (entmakex
      (list
        '(0 . "SOLID")
        '(100 . "AcDbEntity")
        (cons 8 lyr)
        (cons 62 col)
        '(100 . "AcDbTrace")
        (cons 10 (list (car p5) (cadr p5) 0.0))
        (cons 11 (list (car p7) (cadr p7) 0.0))
        (cons 12 (list (car p-mid-base) (cadr p-mid-base) 0.0))
        (cons 13 (list (car p-mid-base) (cadr p-mid-base) 0.0))
      )
    )
  )

  ;; 3. 단면 명칭 TEXT (높이 1000.0, 스타일 "맑은 고딕", 회전각 0, 중간 중심 정렬)
  ;;    [수정] 서브클래스 마커 '(100 . "AcDbText") 는 1회만 사용 (중복 시 entmake 실패 위험)
  (setq e-txt
    (entmakex
      (list
        '(0 . "TEXT")
        '(100 . "AcDbEntity")
        (cons 8 lyr)
        (cons 62 col)
        '(100 . "AcDbText")
        (cons 10 (list (car p-txt) (cadr p-txt) 0.0))
        '(40 . 1000.0)
        (cons 1 label-str)
        '(50 . 0.0)
        '(41 . 1.0)
        '(51 . 0.0)
        '(7 . "맑은 고딕")
        '(71 . 0)
        '(72 . 4)
        (cons 11 (list (car p-txt) (cadr p-txt) 0.0))
        '(73 . 0)
      )
    )
  )
  (list e-pl e-sol e-txt)
)

;; 전개도 상에 수직 단면기호 세트(상단 화살표 + 하단 화살표)를 작도하는 함수
;;   - sec-x    : 전개도 단면 위치 X좌표
;;   - top-ref-y: 전개도 상단 기준 Y좌표 (지표면 또는 주상도 박스 상단)
;;   - bot-ref-y: 전개도 하단 기준 Y좌표 (굴착바닥고 또는 말뚝 하단)
;;   - label-str: 표기할 문자열 (예: "A (Seg-1)")
(defun drg-draw-elevation-section-mark (sec-x top-ref-y bot-ref-y label-str /
                                        p-top-cross p-bot-cross ents1 ents2)
  ;; 상단 기호 교차점: 상단 기준점 위로 3800mm 여유
  (setq p-top-cross (list sec-x (+ top-ref-y 3800.0) 0.0))
  ;; 하단 기호 교차점: 하단 기준점 아래로 3800mm 여유
  (setq p-bot-cross (list sec-x (- bot-ref-y 3800.0) 0.0))

  ;; 상단 기호: 절단선 외측 방향 = 위쪽(+pi/2), 시선 방향 = 우측(0.0)
  (setq ents1 (drg-draw-single-section-symbol p-top-cross (/ pi 2.0) 0.0 label-str))
  ;; 하단 기호: 절단선 외측 방향 = 아래쪽(-pi/2), 시선 방향 = 우측(0.0)
  (setq ents2 (drg-draw-single-section-symbol p-bot-cross (* 1.5 pi) 0.0 label-str))
  (append ents1 ents2)
)

;; 평면도 상에 단면기호 세트(시점 측 화살표 + 종점 측 화살표)를 작도하는 함수
;;   - pt1      : 시작 세그먼트(예: Seg-1) 경계선 위의 단면 통과점
;;   - pt2      : 맞은편 세그먼트(예: Seg-3) 경계선 위의 단면 통과점 (단독 단면이면 nil)
;;   - in-norm1 : pt1 위치에서 굴착 내측을 향하는 법선 각도 (rad)
;;   - letter   : 평면도에 표기할 알파벳 문자 (예: "A", "B")
(defun drg-draw-plan-section-mark (pt1 pt2 in-norm1 letter /
                                   axis-ang out-dir1 out-dir2 view-dir
                                   p-cross1 p-cross2 ents1 ents2)
  (if pt2
    (setq axis-ang (angle (list (car pt1) (cadr pt1)) (list (car pt2) (cadr pt2))))
    (setq axis-ang in-norm1)
  )
  ;; 시점(pt1) 바깥쪽 방향 = axis-ang + pi, 종점(pt2) 바깥쪽 방향 = axis-ang
  (setq out-dir1 (+ axis-ang pi))
  (setq out-dir2 axis-ang)

  ;; 시선 방향(view-dir): 절단선에 수직이면서 도면 우측/상단(코사인>=0 우선)을 향하도록 통일
  (setq view-dir (- axis-ang (/ pi 2.0)))
  (if (< (cos view-dir) -0.01)
    (setq view-dir (+ axis-ang (/ pi 2.0)))
  )

  (if pt2
    (progn
      ;; 관통 단면: 각 세그먼트 경계선에서 배면 바깥쪽으로 2200mm 떨어진 곳에 기호 교차점 배치
      (setq p-cross1 (polar (list (car pt1) (cadr pt1) 0.0) out-dir1 2200.0))
      (setq p-cross2 (polar (list (car pt2) (cadr pt2) 0.0) out-dir2 2200.0))
    )
    (progn
      ;; 단독 단면: 해당 벽체 경계선 전후방 2500mm 위치에 마주 보도록 배치
      (setq p-cross1 (polar (list (car pt1) (cadr pt1) 0.0) out-dir1 2500.0))
      (setq p-cross2 (polar (list (car pt1) (cadr pt1) 0.0) out-dir2 2500.0))
    )
  )

  (setq ents1 (drg-draw-single-section-symbol p-cross1 out-dir1 view-dir letter))
  (setq ents2 (drg-draw-single-section-symbol p-cross2 out-dir2 view-dir letter))
  (append ents1 ents2)
)

;; 전개도 X좌표(elev-x)를 평면도 세그먼트(seg-id) 위의 실제 2D 좌표 및 국부 접선/내측법선 각도로 변환하는 함수
;;   반환: (list plan-pt tang-ang in-norm-ang)
;;   [핵심 개선] 사용자가 전개도에서 H-Pile(측면말뚝)을 클릭한 경우, 경계선 투영 역산 오차가 발생하지 않도록
;;               raw_piles_info에 기록된 '평면도 H-Pile의 실제 중심 좌표(ins_pt)'를 1:1로 정확히 매칭하여 반환함!
(defun drg-elev-x-to-plan-pt (elev-x seg-id seg-list seg-span-map ent-bound is-closed is-forward base-start-dist total-bound-dist bnd-orient /
                              sp c1-x c2-x scale-f seg-obj v-s v-e v-a seg-len u-mm
                              prof-start-x chain target-d pt-on-bnd prm deriv tang-ang orient in-norm-ang
                              matched-pile-pt min-pile-dx p-info px p-pw p-pt tol-x cpt)
  (setq sp (assoc seg-id seg-span-map))
  (setq seg-obj (if (and seg-list (>= seg-id 1) (<= seg-id (length seg-list))) (nth (1- seg-id) seg-list) nil))
  (setq orient (if (numberp bnd-orient) bnd-orient -1))
  (setq prof-start-x (if (assoc 1 seg-span-map) (nth 1 (assoc 1 seg-span-map)) 0.0))

  (if sp
    (progn
      (setq c1-x (float (nth 1 sp))
            c2-x (float (nth 2 sp))
            scale-f (if (numberp (nth 3 sp)) (float (nth 3 sp)) 1.0))

      ;; ★ 0순위: 클릭한 elev-x 가 현재 세그먼트의 H-Pile(측면말뚝) 중심(또는 말뚝 폭 내부)과 일치하는지 검사
      ;;          일치하면 평면도 H-Pile의 실제 삽입점(ins_pt)을 그대로 사용하여 오차를 0으로 만듦!
      (setq matched-pile-pt nil min-pile-dx 1e99)
      (if (and (boundp 'raw_piles_info) raw_piles_info
               (> elev-x (+ c1-x 10.0)) (< elev-x (- c2-x 10.0)))
        (foreach p-info raw_piles_info
          (setq px (float (car p-info))
                p-pw (if (numberp (cadr p-info)) (float (cadr p-info)) 300.0)
                p-pt (caddr p-info))
          (setq tol-x (+ (/ p-pw 2.0) 10.0))
          (if (and p-pt
                   (>= px (- c1-x 1.0)) (<= px (+ c2-x 1.0))
                   (<= (abs (- elev-x px)) tol-x)
                   (< (abs (- elev-x px)) min-pile-dx))
            (setq min-pile-dx (abs (- elev-x px))
                  matched-pile-pt p-pt)
          )
        )
      )

      ;; 1순위: 평면도 경계선 객체(ent-bound)가 있으면 경계선 위에서 접선/법선 각도 및 기본 투영점을 추출
      (if (and ent-bound (entget ent-bound))
        (progn
          (if (or (null total-bound-dist) (<= total-bound-dist 1.0))
            (setq total-bound-dist (vlax-curve-getDistAtParam ent-bound (vlax-curve-getEndParam ent-bound)))
          )
          (if (null base-start-dist) (setq base-start-dist 0.0))

          (if matched-pile-pt
            ;; 매칭된 H-Pile 평면 좌표가 있으면 그 점에서 가장 가까운 경계선 파라미터로 정확한 국부 접선각 계산
            (progn
              (setq cpt (vlax-curve-getClosestPointTo ent-bound matched-pile-pt))
              (setq target-d (vlax-curve-getDistAtPoint ent-bound cpt))
              (setq pt-on-bnd matched-pile-pt)
            )
            (progn
              (setq chain (max 0.0 (min total-bound-dist (- (float elev-x) prof-start-x))))
              (if is-closed
                (progn
                  (setq target-d (if is-forward (+ base-start-dist chain) (- base-start-dist chain)))
                  (while (< target-d 0.0) (setq target-d (+ total-bound-dist target-d)))
                  (while (> target-d total-bound-dist) (setq target-d (- target-d total-bound-dist)))
                )
                (setq target-d (if is-forward chain (- total-bound-dist chain)))
              )
              (setq target-d (max 0.0 (min total-bound-dist target-d)))
              (setq pt-on-bnd (vlax-curve-getPointAtDist ent-bound target-d))
            )
          )

                    ;; 코너(c1-x 또는 c2-x) 정위치를 찍었을 때도 항상 선택된 세그먼트(seg-id) 내부의 접선/법선 각도를 읽도록
          ;; 미분 계산용 거리(deriv-d)는 세그먼트 양 끝단에서 5mm 안쪽으로 클램핑하여 산출
          (if matched-pile-pt
            (setq deriv-d target-d)
            (progn
              (setq seg-span-len (abs (- c2-x c1-x)))
              (setq in-margin (min 5.0 (* 0.1 seg-span-len)))
              (setq deriv-x (max (+ c1-x in-margin) (min (- c2-x in-margin) (float elev-x))))
              (setq deriv-chain (max 0.0 (min total-bound-dist (- deriv-x prof-start-x))))
              (if is-closed
                (progn
                  (setq deriv-d (if is-forward (+ base-start-dist deriv-chain) (- base-start-dist deriv-chain)))
                  (while (< deriv-d 0.0) (setq deriv-d (+ total-bound-dist deriv-d)))
                  (while (> deriv-d total-bound-dist) (setq deriv-d (- deriv-d total-bound-dist)))
                )
                (setq deriv-d (if is-forward deriv-chain (- total-bound-dist deriv-chain)))
              )
              (setq deriv-d (max 0.0 (min total-bound-dist deriv-d)))
            )
          )
          (setq prm (vlax-curve-getParamAtDist ent-bound deriv-d))
          (if (and prm (equal prm (fix (+ prm 0.5)) 1e-4))
            (if (>= prm (- (vlax-curve-getEndParam ent-bound) 1e-4))
              (setq prm (max 0.0 (- prm 1e-3)))
              (setq prm (+ prm 1e-3))
            )
          )
          (setq deriv (if prm (vlax-curve-getFirstDeriv ent-bound prm) '(1.0 0.0 0.0)))
          (setq tang-ang (angle '(0.0 0.0 0.0) deriv))
          (if (not is-forward) (setq tang-ang (+ tang-ang pi)))
          (setq in-norm-ang (+ tang-ang (* orient (/ pi 2.0))))
          (if pt-on-bnd
            (list (list (car pt-on-bnd) (cadr pt-on-bnd) 0.0) tang-ang in-norm-ang)
            nil
          )
        )
        ;; 2순위: ent-bound가 없을 때는 세그먼트의 V-START / V-END 로 계산
        (if seg-obj
          (progn
            (setq v-s (cdr (assoc 'V-START seg-obj))
                  v-e (cdr (assoc 'V-END seg-obj))
                  v-a (cdr (assoc 'ANGLE seg-obj)))
            (if (and (null v-a) v-s v-e)
              (setq v-a (angle (list (car v-s) (cadr v-s)) (list (car v-e) (cadr v-e))))
            )
            (if (null v-a) (setq v-a 0.0))
            (setq seg-len
              (cond
                ((numberp (cdr (assoc 'LENGTH seg-obj))) (float (cdr (assoc 'LENGTH seg-obj))))
                ((and v-s v-e) (distance (list (car v-s) (cadr v-s)) (list (car v-e) (cadr v-e))))
                (t (abs (- c2-x c1-x)))
              )
            )
            (setq u-mm (if (> (abs scale-f) 1e-6) (/ (- elev-x c1-x) scale-f) (- elev-x c1-x)))
            (setq u-mm (max 0.0 (min seg-len u-mm)))
            (if matched-pile-pt
              (progn
                (setq tang-ang v-a)
                (setq in-norm-ang (+ tang-ang (* orient (/ pi 2.0))))
                (list (list (car matched-pile-pt) (cadr matched-pile-pt) 0.0) tang-ang in-norm-ang)
              )
              (if v-s
                (progn
                  (setq pt-on-bnd (polar (list (car v-s) (cadr v-s) 0.0) v-a u-mm))
                  (setq tang-ang v-a)
                  (setq in-norm-ang (+ tang-ang (* orient (/ pi 2.0))))
                  (list (list (car pt-on-bnd) (cadr pt-on-bnd) 0.0) tang-ang in-norm-ang)
                )
                nil
              )
            )
          )
          nil
        )
      )
    )
    nil
  )
)

;; 평면도 세그먼트(t-seg-id) 위의 점(pt)을 전개도 X좌표로 변환하는 함수
(defun drg-plan-pt-to-elev-x (pt t-seg-id seg-list seg-span-map ent-bound is-closed is-forward base-start-dist total-bound-dist /
                              prof-start-x cpt cur-d ch t-sp t-seg tv-s tv-e tv-a t-len t-c1-x t-c2-x t-scale dx dy u-proj)
  (setq prof-start-x (if (assoc 1 seg-span-map) (nth 1 (assoc 1 seg-span-map)) 0.0))
  (setq t-sp (assoc t-seg-id seg-span-map))
  (if (and ent-bound (entget ent-bound))
    (progn
      (if (or (null total-bound-dist) (<= total-bound-dist 1.0))
        (setq total-bound-dist (vlax-curve-getDistAtParam ent-bound (vlax-curve-getEndParam ent-bound)))
      )
      (if (null base-start-dist) (setq base-start-dist 0.0))
      (setq cpt (vlax-curve-getClosestPointTo ent-bound (list (car pt) (cadr pt) 0.0)))
      (setq cur-d (vlax-curve-getDistAtPoint ent-bound cpt))
      (if is-closed
        (progn
          (setq ch (if is-forward (- cur-d base-start-dist) (- base-start-dist cur-d)))
          (if (< (abs ch) 0.01) (setq ch 0.0))
          (while (< ch -0.01) (setq ch (+ total-bound-dist ch)))
          (while (> ch (+ total-bound-dist 0.01)) (setq ch (- ch total-bound-dist)))
        )
        (setq ch (if is-forward cur-d (- total-bound-dist cur-d)))
      )
      (if t-sp
        (max (float (nth 1 t-sp)) (min (float (nth 2 t-sp)) (+ prof-start-x ch)))
        (+ prof-start-x ch)
      )
    )
    (progn
      (setq t-seg (if (and seg-list (>= t-seg-id 1) (<= t-seg-id (length seg-list))) (nth (1- t-seg-id) seg-list) nil))
      (if (and t-sp t-seg)
        (progn
          (setq tv-s (cdr (assoc 'V-START t-seg))
                tv-e (cdr (assoc 'V-END t-seg))
                tv-a (cdr (assoc 'ANGLE t-seg))
                t-c1-x (float (nth 1 t-sp))
                t-c2-x (float (nth 2 t-sp))
                t-scale (if (numberp (nth 3 t-sp)) (float (nth 3 t-sp)) 1.0))
          (if (and (null tv-a) tv-s tv-e)
            (setq tv-a (angle (list (car tv-s) (cadr tv-s)) (list (car tv-e) (cadr tv-e))))
          )
          (if (null tv-a) (setq tv-a 0.0))
          (setq t-len
            (cond
              ((numberp (cdr (assoc 'LENGTH t-seg))) (float (cdr (assoc 'LENGTH t-seg))))
              ((and tv-s tv-e) (distance (list (car tv-s) (cadr tv-s)) (list (car tv-e) (cadr tv-e))))
              (t (abs (- t-c2-x t-c1-x)))
            )
          )
          (if tv-s
            (progn
              (setq dx (- (car pt) (car tv-s))
                    dy (- (cadr pt) (cadr tv-s)))
              (setq u-proj (max 0.0 (min t-len (+ (* dx (cos tv-a)) (* dy (sin tv-a))))))
              (+ t-c1-x (* u-proj t-scale))
            )
            (/ (+ t-c1-x t-c2-x) 2.0)
          )
        )
        nil
      )
    )
  )
)

;; 시작 세그먼트(src-seg-id)의 평면 기준점(pt1) 또는 전개도 좌표에서 맞은편 세그먼트와 교차하는
;; (list opp-seg-id opp-plan-pt opp-elev-x ray-ang)를 반환하는 무결점 함수
;; [핵심 개선]
;; 1) 미정의 세그먼트(IS-DEFINED = nil)의 V-START/V-END/ANGLE이 비어 있더라도,
;;    평면도 경계선(ent-bound)과 seg-span-map에서 직접 각 세그먼트의 실제 2D 선분(sp1~sp2)을 복원하여 추적!
;; 2) 1차 광선 추적 + 2차 대향 벽체 수선의 발 투영 + 3차 직사각형/ㄷ자 마주보는 세그먼트(길이가 유사한 |seg-id - opp-id| = 2 벽체) 대칭 역산까지
;;    3중 안전장치를 적용하여 어떤 상황에서도 맞은편 관통 벽체를 100% 찾아냄.
(defun drg-find-opposite-section-hit (src-seg-id pt1 local-in-norm seg-list seg-span-map ent-bound is-closed is-forward base-start-dist total-bound-dist bnd-orient /
                                      src-seg sup-lst strut-item brace-opt ang-off-deg ray-ang cand-angs
                                      sg s bo off-d r1 r2
                                      seg-geom-list sp-item sid sc1 sc2 res1 res2 gpt1 gpt2 gang
                                      best-tid best-d best-int-pt best-ang p-ray
                                      g-item t-seg-id sp1 sp2 seg-ang int-pt d-ray
                                      src-geom src-sp1 src-sp2 src-ang opp-seg-ang seg-len dx dy u-proj proj-pt
                                      src-sp src-c1 src-c2 opp-x)
  (setq src-seg (if (and seg-list (>= src-seg-id 1) (<= src-seg-id (length seg-list))) (nth (1- src-seg-id) seg-list) nil))
  (setq sup-lst (if src-seg (cdr (assoc 'SUPPORT-LIST src-seg)) nil))
  (setq strut-item nil)
  (foreach s sup-lst
    (if (and (null strut-item) (listp s) (> (length s) 3) (wcmatch (car s) "버팀보*"))
      (setq strut-item s)
    )
  )

  (setq brace-opt (if (and strut-item (> (length strut-item) 11)) (nth 11 strut-item) nil))
  (setq ang-off-deg (if (and (listp brace-opt) (> (length brace-opt) 14) (nth 14 brace-opt))
                      (atof (vl-princ-to-string (nth 14 brace-opt)))
                      0.0))
  (setq ray-ang (+ local-in-norm (* ang-off-deg (/ pi 180.0))))

  (setq cand-angs (list ray-ang (+ ray-ang pi) local-in-norm (+ local-in-norm pi)))
  (foreach sg seg-list
    (foreach s (cdr (assoc 'SUPPORT-LIST sg))
      (if (and (listp s) (> (length s) 11) (wcmatch (car s) "버팀보*"))
        (progn
          (setq bo (nth 11 s))
          (if (and (listp bo) (> (length bo) 14) (nth 14 bo))
            (progn
              (setq off-d (atof (vl-princ-to-string (nth 14 bo))))
              (if (> (abs off-d) 1e-4)
                (progn
                  (setq r1 (+ local-in-norm (* off-d (/ pi 180.0))))
                  (setq r2 (- local-in-norm (* off-d (/ pi 180.0))))
                  (setq cand-angs (append cand-angs (list r1 (+ r1 pi) r2 (+ r2 pi))))
                )
              )
            )
          )
        )
      )
    )
  )

  ;; ★ 핵심: seg-span-map 의 각 세그먼트 시작X(sc1) ~ 끝X(sc2)를 평면도 좌표(gpt1 ~ gpt2)로 직접 변환하여
  ;;         미정의 세그먼트라도 100% 정확한 평면 선분 데이터를 확보함!
  (setq seg-geom-list nil)
  (foreach sp-item seg-span-map
    (setq sid (nth 0 sp-item)
          sc1 (float (nth 1 sp-item))
          sc2 (float (nth 2 sp-item)))
    (setq res1 (drg-elev-x-to-plan-pt sc1 sid seg-list seg-span-map ent-bound is-closed is-forward base-start-dist total-bound-dist bnd-orient))
    (setq res2 (drg-elev-x-to-plan-pt sc2 sid seg-list seg-span-map ent-bound is-closed is-forward base-start-dist total-bound-dist bnd-orient))
    (if (and res1 res2 (nth 0 res1) (nth 0 res2))
      (progn
        (setq gpt1 (nth 0 res1)
              gpt2 (nth 0 res2))
        (if (> (distance gpt1 gpt2) 1.0)
          (progn
            (setq gang (angle gpt1 gpt2))
            (setq seg-geom-list (append seg-geom-list (list (list sid gpt1 gpt2 gang))))
          )
        )
      )
    )
  )

  (setq best-tid nil best-d 1e99 best-int-pt nil best-ang ray-ang)
  (if pt1 (setq pt1 (list (car pt1) (cadr pt1) 0.0)))

  ;; [1차 탐색] 광선 교차(Ray-casting) 탐색
  (if (and pt1 seg-geom-list)
    (foreach try-ang cand-angs
      (if (null best-tid)
        (progn
          (setq p-ray (polar pt1 try-ang 1e6))
          (foreach g-item seg-geom-list
            (setq t-seg-id (nth 0 g-item)
                  sp1      (nth 1 g-item)
                  sp2      (nth 2 g-item)
                  seg-ang  (nth 3 g-item))
            (if (/= t-seg-id src-seg-id)
              (progn
                (setq int-pt (inters pt1 p-ray sp1 sp2 T))
                (if int-pt
                  (progn
                    (setq d-ray (distance pt1 int-pt))
                    ;; 인접 모서리 오인식 방지(d-ray > 300mm) 및 광선-벽체 교차각 25도 이상(|sin| >= 0.42) 확인
                    (if (and (> d-ray 300.0)
                             (< d-ray best-d)
                             (>= (abs (sin (- try-ang seg-ang))) 0.42))
                      (setq best-d d-ray
                            best-tid t-seg-id
                            best-int-pt int-pt
                            best-ang try-ang)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  ;; [2차 보조 탐색] 현재 세그먼트와 마주보는(평행/대향, 각도차 45도 이내) 세그먼트 선분에 수선의 발 투영
  (if (and (null best-tid) pt1 seg-geom-list)
    (progn
      (setq src-geom (assoc src-seg-id seg-geom-list))
      (setq src-ang (if src-geom (nth 3 src-geom) (if (and src-seg (cdr (assoc 'ANGLE src-seg))) (cdr (assoc 'ANGLE src-seg)) (+ local-in-norm (/ pi 2.0)))))
      (foreach g-item seg-geom-list
        (setq t-seg-id    (nth 0 g-item)
              sp1         (nth 1 g-item)
              sp2         (nth 2 g-item)
              opp-seg-ang (nth 3 g-item))
        (if (/= t-seg-id src-seg-id)
          ;; 두 벽체가 서로 마주보거나 나란한 방향(|sin(각도차)| <= sin(45도) = 0.7071)인지 확인
          (if (<= (abs (sin (- src-ang opp-seg-ang))) 0.7071)
            (progn
              (setq seg-len (distance sp1 sp2))
              (setq dx (- (car pt1) (car sp1))
                    dy (- (cadr pt1) (cadr sp1)))
              (setq u-proj (+ (* dx (cos opp-seg-ang)) (* dy (sin opp-seg-ang))))
              (if (and (>= u-proj -1000.0) (<= u-proj (+ seg-len 1000.0)))
                (progn
                  (setq u-proj (max 0.0 (min seg-len u-proj)))
                  (setq proj-pt (polar sp1 opp-seg-ang u-proj))
                  (setq d-ray (distance pt1 proj-pt))
                  (if (and (> d-ray 300.0) (< d-ray best-d))
                    (setq best-d d-ray
                          best-tid t-seg-id
                          best-int-pt proj-pt
                          best-ang (angle pt1 proj-pt))
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  (if (and best-tid best-int-pt)
    (progn
      (setq opp-x (drg-plan-pt-to-elev-x best-int-pt best-tid seg-list seg-span-map ent-bound is-closed is-forward base-start-dist total-bound-dist))
      (if opp-x
        (list best-tid best-int-pt opp-x best-ang)
        nil
      )
    )
    nil
  )
)

;; [3차 안전장치] 평면도 경계선(ent-bound)이 없거나 좌표 변환이 불가능한 상태에서도
;; ㄷ자(3개 구간: 1<->3) 또는 직사각형(4개 구간: 1<->3, 2<->4) 구조에서 마주보는 벽체의 전개도 대칭 X좌표를 역산하는 보조 함수
(defun drg-fallback-opposite-by-span (src-seg-id src-x seg-list seg-span-map ent-bound is-closed is-forward base-start-dist total-bound-dist bnd-orient /
                                      cand-id src-sp opp-sp sc1 sc2 oc1 oc2 s-len o-len dist-from-start opp-x opp-plan-res opp-pt)
  (setq cand-id
    (cond
      ((<= (+ src-seg-id 2) (length seg-span-map)) (+ src-seg-id 2))
      ((>= (- src-seg-id 2) 1) (- src-seg-id 2))
      (t nil)
    )
  )
  (setq src-sp (assoc src-seg-id seg-span-map))
  (setq opp-sp (if cand-id (assoc cand-id seg-span-map) nil))
  (if (and src-sp opp-sp)
    (progn
      (setq sc1 (float (nth 1 src-sp)) sc2 (float (nth 2 src-sp)) s-len (- sc2 sc1))
      (setq oc1 (float (nth 1 opp-sp)) oc2 (float (nth 2 opp-sp)) o-len (- oc2 oc1))
      ;; 마주보는 벽체는 진행 방향이 반대이므로, 시작점으로부터의 거리(dist-from-start)는 맞은편 벽체의 끝점(oc2)으로부터의 거리에 대응됨
      (setq dist-from-start (- (float src-x) sc1))
      (setq opp-x (max oc1 (min oc2 (- oc2 dist-from-start))))
      (setq opp-plan-res (drg-elev-x-to-plan-pt opp-x cand-id seg-list seg-span-map ent-bound is-closed is-forward base-start-dist total-bound-dist bnd-orient))
      (setq opp-pt (if opp-plan-res (nth 0 opp-plan-res) nil))
      (list cand-id opp-pt opp-x 0.0)
    )
    nil
  )
)

;; --- [4] 전개도 전용 지반 주상도 블록('HOLE-NO(프로젝트명_세그먼트명)') 생성 및 삽입 함수 ---
;;   - AutoCAD 블록 이름 금지 문자(< > / \ " : ; ? * | , =)를 언더바(_)로 안전하게 치환
(defun drg-sanitize-block-name (str / i ch res)
  (setq res "" i 1)
  (while (<= i (strlen str))
    (setq ch (substr str i 1))
    (if (wcmatch ch "[<>/\\\\\":;?*|=`]")
      (setq res (strcat res "_"))
      (setq res (strcat res ch))
    )
    (setq i (1+ i))
  )
  (if (= res "") "SOIL_COL" res)
)

;;   - 소수점 둘째 자리 포맷팅 (끝자리 0 유지, 예: 316.60)
(defun drg-format-el-2dec (val / old-dz res)
  (setq old-dz (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq res (rtos (float val) 2 2))
  (setvar "DIMZIN" old-dz)
  res
)

;;   - 소수점 첫째 자리 상대 심도 포맷팅 (예: 2.5, 5.0)
(defun drg-format-depth-1dec (val / old-dz res)
  (setq old-dz (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq res (rtos (float val) 2 1))
  (setvar "DIMZIN" old-dz)
  res
)

;;   - TSP_3A_Section.lsp 의 draw-soil-column 과 100% 동일한 구성으로 작도 후
;;     기준점 (center-x, top-y) = HOLE-NO 박스 하단 수직선 끝점을 블록 기준점으로 하여
;;     'HOLE-NO(프로젝트명_세그먼트명)' 블록으로 생성 및 삽입하고 INSERT 엔티티를 반환
(defun drg-create-and-insert-soil-column-block (center-x top-y top-el-val seg-obj proj-name /
                                                soil-layers n-values hole-no water-chk water-depth
                                                seg-name raw-blk-name base-blk-name blk-name suffix-idx
                                                layer-name scale-sec s-factor col-width txt-h leader-len txt-off-l txt-off-c
                                                start-x top-left-pt col-x-right mark-ent prev-depth
                                                wl-y wl-x wl-pt line1-len line1-start line1-end line1-mid
                                                l2-half off-y1 line2-y line2-start line2-end l3-half off-y2 line3-y line3-start line3-end
                                                tri-h tri-w-half tri-p1 tri-p2 tri-p3 tri-p_top_center text-gap text-str text-pt text-h
                                                layer l-name l-depth l-pattern l-scale l-angle curr-depth y-prev y-curr
                                                pt1 pt2 pt3 pt4 center-pt rect-pl base-scale final-scale final-angle
                                                text-x text-y-name h-txt-h center-top-x center-top-y line-top-y box-hw box-h p1 p2 p3 p4 text-y
                                                n-item n-depth n-str n-y n-pt1 n-pt2 n-text-pt
                                                new-ents nxt doc blocks-col blk-def vla-objs sa ins-ent)
  (setq soil-layers (cdr (assoc 'SOIL-DATA seg-obj)))
  (if (and soil-layers (> (length soil-layers) 0))
    (progn
      (setq n-values (cdr (assoc 'N-VALUES seg-obj)))
      (setq hole-no (cdr (assoc 'HOLE-NO seg-obj)))
      (if (or (null hole-no) (= hole-no ""))
        (setq hole-no (strcat "BH-" (itoa (cdr (assoc 'ID seg-obj)))))
      )
      (setq water-chk (cdr (assoc 'WATER-CHK seg-obj)))
      (if (null water-chk) (setq water-chk "0"))
      (setq water-depth (cdr (assoc 'WATER-DEPTH seg-obj)))
      (if (null water-depth) (setq water-depth 0.0))
      (if (= (type water-depth) 'STR) (setq water-depth (atof water-depth)))

      (setq seg-name (cdr (assoc 'NAME seg-obj)))
      (if (or (null seg-name) (= seg-name ""))
        (setq seg-name (strcat "Seg-" (itoa (cdr (assoc 'ID seg-obj)))))
      )
      (if (or (null proj-name) (= proj-name ""))
        (setq proj-name (if (and (boundp '*tsp-current-project-name*) *tsp-current-project-name* (/= *tsp-current-project-name* ""))
                          *tsp-current-project-name*
                          "Project"))
      )

      ;; 블록 이름: BH-1(프로젝트명_세그먼트명)
      (setq raw-blk-name (strcat hole-no "(" proj-name "_" seg-name ")"))
      (setq base-blk-name (drg-sanitize-block-name raw-blk-name))
      (setq blk-name base-blk-name suffix-idx 2)
      (while (tblsearch "BLOCK" blk-name)
        (setq blk-name (strcat base-blk-name "_" (itoa suffix-idx)))
        (setq suffix-idx (1+ suffix-idx))
      )

      (setq layer-name "_지반선")
      (if (not (tblsearch "LAYER" layer-name))
        (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") (cons 2 layer-name) '(70 . 0) '(62 . 8)))
      )

      (setq scale-sec (if (and (boundp '*tsp-scale-section*) *tsp-scale-section* (> *tsp-scale-section* 0))
                        (float *tsp-scale-section*)
                        200.0))
      (setq s-factor (/ scale-sec 200.0))
      (setq col-width (* 1000.0 s-factor))
      (setq txt-h (* 600.0 s-factor))
      (setq leader-len (* 1000.0 s-factor))
      (setq txt-off-l (* 1200.0 s-factor))
      (setq txt-off-c (* 3500.0 s-factor))

      ;; 기둥 중심(center-top-x = 시추공 박스의 수직선 위치)이 정확히 center-x(단면선택점)에 일치하도록 start-x 산출
      (setq start-x (- center-x (/ col-width 2.0)))
      (setq top-left-pt (list start-x top-y 0.0))
      (setq col-x-right (+ start-x col-width))

      (setq mark-ent (entlast))

      ;; 1. 최상단 지시선 및 실제 표고(소수점 2자리, 0 유지) 텍스트
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                     (cons 8 layer-name) '(62 . 8) '(90 . 2) '(70 . 0)
                     (cons 10 (list (car top-left-pt) (cadr top-left-pt)))
                     (cons 10 (list (- (car top-left-pt) leader-len) (cadr top-left-pt)))))
      (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText")
                     (cons 8 layer-name) '(62 . 8)
                     (cons 10 (list (- (car top-left-pt) txt-off-l) (cadr top-left-pt) 0.0))
                     (cons 40 txt-h)
                     (cons 1 (drg-format-el-2dec top-el-val))
                     '(72 . 2)
                     (cons 11 (list (- (car top-left-pt) txt-off-l) (cadr top-left-pt) 0.0))))

      ;; 2. 지하수위 기호 및 텍스트 (draw-water-level-symbol 동일 로직)
      (if (= water-chk "1")
        (progn
          (setq wl-y (- (cadr top-left-pt) (* water-depth 1000.0)))
          (setq wl-x (+ col-x-right (* 3000.0 s-factor)))
          (setq wl-pt (list wl-x wl-y 0.0))
          (if (boundp 'draw-water-level-symbol)
            (draw-water-level-symbol wl-pt water-depth wl-x)
            (progn
              (setq tri-h (* 800.0 s-factor))
              (setq tri-w-half (/ tri-h (sqrt 3.0)))
              (setq line1-len (* 3000.0 s-factor))
              (setq line1-start (list wl-x (cadr wl-pt) 0.0))
              (setq line1-end (list (+ wl-x line1-len) (cadr wl-pt) 0.0))
              (setq line1-mid (list (/ (+ (car line1-start) (car line1-end)) 2.0) (cadr wl-pt) 0.0))
              (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) '(62 . 8) '(90 . 2) '(70 . 0) (cons 10 line1-start) (cons 10 line1-end)))
              (setq l2-half (* 750.0 s-factor) off-y1 (* 100.0 s-factor) line2-y (- (cadr line1-mid) off-y1))
              (setq line2-start (list (- (car line1-mid) l2-half) line2-y 0.0) line2-end (list (+ (car line1-mid) l2-half) line2-y 0.0))
              (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) '(62 . 8) '(90 . 2) '(70 . 0) (cons 10 line2-start) (cons 10 line2-end)))
              (setq l3-half (* 500.0 s-factor) off-y2 (* 200.0 s-factor) line3-y (- (cadr line1-mid) off-y2))
              (setq line3-start (list (- (car line1-mid) l3-half) line3-y 0.0) line3-end (list (+ (car line1-mid) l3-half) line3-y 0.0))
              (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) '(62 . 8) '(90 . 2) '(70 . 0) (cons 10 line3-start) (cons 10 line3-end)))
              (setq tri-p3 line1-mid)
              (setq tri-p1 (list (- (car line1-mid) tri-w-half) (+ (cadr line1-mid) tri-h) 0.0))
              (setq tri-p2 (list (+ (car line1-mid) tri-w-half) (+ (cadr line1-mid) tri-h) 0.0))
              (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) '(62 . 8) '(90 . 3) '(70 . 1) (cons 10 tri-p1) (cons 10 tri-p2) (cons 10 tri-p3)))
              (setq tri-p_top_center (list (car line1-mid) (+ (cadr line1-mid) tri-h) 0.0))
              (entmake (list '(0 . "SOLID") '(100 . "AcDbEntity") '(100 . "AcDbTrace") (cons 8 layer-name) '(62 . 8) (cons 10 tri-p1) (cons 11 tri-p_top_center) (cons 12 tri-p3) (cons 13 tri-p3)))
              (setq text-gap (* 400.0 s-factor))
              (setq text-str (strcat "-" (drg-format-depth-1dec water-depth) "m"))
              (setq text-pt (list (car line1-mid) (+ (cadr line1-mid) tri-h text-gap) 0.0))
              (setq text-h (* 600.0 s-factor))
              (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText") (cons 8 layer-name) '(62 . 8) (cons 10 text-pt) (cons 40 text-h) (cons 1 text-str) '(72 . 1) (cons 11 text-pt)))
            )
          )
        )
      )

      ;; 3. 각 지층별 기둥, 커스텀 해치, 좌측 상대 심도(m) 및 지층명 표기
      (setq prev-depth 0.0)
      (foreach layer soil-layers
        (setq l-name (car layer)
              l-depth (float (cadr layer))
              l-pattern (nth 2 layer)
              l-scale (nth 3 layer)
              l-angle (nth 4 layer))
        (setq curr-depth (* l-depth 1000.0))
        (setq y-prev (- (cadr top-left-pt) prev-depth))
        (setq y-curr (- (cadr top-left-pt) curr-depth))
        (setq pt1 (list (car top-left-pt) y-prev))
        (setq pt2 (list (+ (car top-left-pt) col-width) y-prev))
        (setq pt3 (list (+ (car top-left-pt) col-width) y-curr))
        (setq pt4 (list (car top-left-pt) y-curr))
        (setq center-pt (list (/ (+ (car pt1) (car pt3)) 2.0) (/ (+ (cadr pt1) (cadr pt3)) 2.0) 0.0))

        (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                       (cons 8 layer-name) '(62 . 8) '(90 . 4) '(70 . 1)
                       (cons 10 pt1) (cons 10 pt2) (cons 10 pt3) (cons 10 pt4)))
        (setq rect-pl (entlast))

        (setq base-scale (if (and l-scale (> l-scale 0)) (float l-scale) 100.0))
        (setq final-scale (* base-scale s-factor))
        (setq final-angle (if l-angle (float l-angle) 0.0))
        (if (and rect-pl (boundp 'tsp-apply-custom-hatch))
          (tsp-apply-custom-hatch rect-pl l-pattern final-scale (* final-angle (/ pi 180.0)) layer-name 8 center-pt)
        )

        ;; 지층 하단 경계 지시선 및 상대 굴착 심도(예: 2.5, 5.0) 표기
        (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                       (cons 8 layer-name) '(62 . 8) '(90 . 2) '(70 . 0)
                       (cons 10 pt4) (cons 10 (list (- (car pt4) leader-len) (cadr pt4)))))
        (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText")
                       (cons 8 layer-name) '(62 . 8)
                       (cons 10 (list (- (car pt4) txt-off-l) (cadr pt4) 0.0))
                       (cons 40 txt-h)
                       (cons 1 (if (boundp 'format-depth-text) (format-depth-text l-depth) (drg-format-depth-1dec l-depth)))
                       '(72 . 2)
                       (cons 11 (list (- (car pt4) txt-off-l) (cadr pt4) 0.0))))

        ;; 지층명 표기
        (setq text-x (- (+ (car pt1) (/ col-width 2.0)) txt-off-c))
        (setq text-y-name (- (/ (+ y-prev y-curr) 2.0) (/ txt-h 2.0)))
        (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText")
                       (cons 8 layer-name) '(62 . 8)
                       (cons 10 (list text-x text-y-name 0.0))
                       (cons 40 txt-h)
                       (cons 1 l-name)
                       '(72 . 2)
                       (cons 11 (list text-x text-y-name 0.0))))
        (setq prev-depth curr-depth)
      )

      ;; 4. 상단 시추공 번호(HOLE No.) 박스 및 중심 수직선 (하단 끝점 = center-x, top-y)
      (setq h-txt-h (* 500.0 s-factor))
      (setq center-top-x (+ start-x (/ col-width 2.0)))
      (setq center-top-y (cadr top-left-pt))
      (setq line-top-y (+ center-top-y (* 1000.0 s-factor)))

      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                     (cons 8 layer-name) '(62 . 8) '(90 . 2) '(70 . 0)
                     (cons 10 (list center-top-x center-top-y))
                     (cons 10 (list center-top-x line-top-y))))

      (setq box-hw (* 1100.0 s-factor))
      (setq box-h (* 800.0 s-factor))
      (setq p1 (list (- center-top-x box-hw) line-top-y))
      (setq p2 (list (+ center-top-x box-hw) line-top-y))
      (setq p3 (list (+ center-top-x box-hw) (+ line-top-y box-h)))
      (setq p4 (list (- center-top-x box-hw) (+ line-top-y box-h)))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                     (cons 8 layer-name) '(62 . 8) '(90 . 4) '(70 . 1)
                     (cons 10 p1) (cons 10 p2) (cons 10 p3) (cons 10 p4)))

      (setq text-y (+ line-top-y (* 400.0 s-factor)))
      (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText")
                     (cons 8 layer-name) '(62 . 8)
                     (cons 10 (list center-top-x text-y 0.0))
                     (cons 40 h-txt-h)
                     (cons 1 hole-no)
                     '(72 . 1) (cons 11 (list center-top-x text-y 0.0)) '(73 . 2)))

      ;; 5. 우측 N치 지시선 및 텍스트
      (if n-values
        (progn
          (setq leader-len (* 2000.0 s-factor))
          (foreach n-item n-values
            (setq n-depth (* (float (car n-item)) 1000.0))
            (setq n-str (cadr n-item))
            (setq n-y (- (cadr top-left-pt) n-depth))
            (setq n-pt1 (list col-x-right n-y))
            (setq n-pt2 (list (+ col-x-right leader-len) n-y))
            (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                           (cons 8 layer-name) '(62 . 8) '(90 . 2) '(70 . 0)
                           (cons 10 n-pt1) (cons 10 n-pt2)))
            (setq n-text-pt (list (+ (car n-pt1) (* 100.0 s-factor)) (+ n-y (* 150.0 s-factor)) 0.0))
            (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText")
                           (cons 8 layer-name) '(62 . 8)
                           (cons 10 n-text-pt)
                           (cons 40 txt-h)
                           (cons 1 n-str)))
          )
        )
      )

      ;; 6. 방금 생성된 주상도 구성 객체들을 수집하여 (center-x, top-y, 0.0) 기준점의 블록으로 변환 및 삽입
      (setq new-ents nil)
      (setq nxt (if mark-ent (entnext mark-ent) (entnext)))
      (while nxt
        (setq new-ents (append new-ents (list nxt)))
        (setq nxt (entnext nxt))
      )

      (if new-ents
        (progn
          (setq doc (vla-get-activedocument (vlax-get-acad-object)))
          (setq blocks-col (vla-get-blocks doc))
          (setq blk-def (vl-catch-all-apply 'vla-add (list blocks-col (vlax-3d-point (list center-x top-y 0.0)) blk-name)))
          (if (not (vl-catch-all-error-p blk-def))
            (progn
              (setq vla-objs (mapcar 'vlax-ename->vla-object new-ents))
              (setq sa (vlax-make-safearray vlax-vbObject (cons 0 (1- (length vla-objs)))))
              (vlax-safearray-fill sa vla-objs)
              (vl-catch-all-apply 'vla-copyobjects (list doc sa blk-def))
              (foreach e new-ents
                (if (and e (entget e)) (vl-catch-all-apply 'entdel (list e)))
              )
              (setq ins-ent
                (entmakex
                  (list '(0 . "INSERT")
                        '(100 . "AcDbEntity")
                        (cons 8 layer-name)
                        '(62 . 256)
                        '(100 . "AcDbBlockReference")
                        (cons 2 blk-name)
                        (cons 10 (list center-x top-y 0.0))
                        '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) '(50 . 0.0))
                )
              )
              ins-ent
            )
            nil
          )
        )
        nil
      )
    )
    nil
  )
)

(defun drg-run-elevation-with-tsp (in-seg-list in-bnd-ent in-bnd-orient /
                                   seg-list bnd-ent bnd-orient saved-projs dcl-path csv-f
                                   get-y-on-pline draw-capsule-shape draw-bottom-capsule is-pt-in-poly get-pile-size-from-name
                                   get-max-y-on-pline calc-pt-chain-on-bound find-pile-idx-near-x
                                   ss_lines raw_list i unique_list deleted_count p_start p_end is_dup u_start u_end
                                   line_list avg_y ent_grnd ent_excav ent_hrz
                                   hrz_p1 hrz_p2 hrz_len prof_start_x final_target_x
                                   place_opt ss_plan ent_bound_sel ent_bound total_bound_dist is_closed
                                   tsp_start_pt v0 v_last base_start_param base_start_dist v1_pt dir_param dir_dist dist_diff is_forward
                                   k cur_dist chain
                                   plan_capsules plan_circles plan_texts plan_lines plan_polys p_ent p_data p_type p_layer
                                   poly_text_alist found_txt a72 a73 t_pt c_num min_in dist c_center c_rad
                                   cap_list connected_polys active_pts changed avail_lines p1 p2 is_connected remain_lines cpt1 cpt2 txt
                                   corner_x_list corner_info_list seg_idx num_segs cur_seg v_pt prev_chain c_x total_seg_len scale_len cum_len
                                   max_grnd_y ss_exist_joints j_ent j_data j_cen j_rad max_joint_top_y def_txt_hgt def_joint_rad
                                   item_idx total_c c_item e_num e_x match_caps caps_to_draw near_idx last_p_idx
                                   base_y cur_y red_line_start red_line_end rev_caps cap_idx num_caps cap_h_half cap_c_rad cap_center_y cap_center cap_w_half
                                   red_short_start red_short_end yel_line_start yel_line_end top_circle_rad top_circle_center
                                   raw_piles_info blk_name ext_size ins_pt cpt
                                   tsp_spans drawn_piles_info sp sec_start_x sec_end_x cur_depth def_pw ctc cur_piles_info
                                   p_info px pw last_pile_x curr_x ext_pw cur_pw top_y bot_y
                                   existing_pile e_idx found_idx ep old_top_y old_bot_y old_pw old_poly old_line old_ghost
                                   final_bot_y final_pw half_w pt1 pt2 pt3 pt4 new_poly new_line new_ep temp_list
                                   end_circle_x ghost_exists first_p g_bot_y g_pw g_top_y g_poly g_line ep_poly ep_line
                                   final_piles pile_idx p_x p_bot_y gap l_len l_start l_end cur_txt_hgt cur_txt_sty cap_rad
                                   max_cap_num seg_ref_cy_map seg_ref_pt_map seg_id seg_c1_x seg_c2_x mid_x ref_pt ref_x ref_cy
                                   hatch_ents block_ents soil_col_ents w_type max_d wale_sp sup_lst soil_lst rock_emb c_brace scale_f
                                   rock_d rock_y user_emb_mm cip_bot_y pts_timber pts_sc pts_cip
                                   proj_sup_data proj_strut_xs proj_corner_xs proj_anchor_xs
                                   s_item s_type s_depth ins_y item_wale_sp wale_b is_2h_wale
                                   sup_x1 sup_x2 r_pts iv_d1 iv_d2 seg_obj seg_v_s seg_v_a seg_len_mm dx dy dp1 dp2
                                   s_cnt s_spec is_2h_strut s_ctc_mm cur_sx blk_to_use c_rows c_is_2h ss_back ss_soil_back ss_front b_ent
                                   seg_span_map prev_idx prev_seg_id next_seg_id prev_sp next_sp
                                   c_stages st st_depth st_rows max_b_dist max_row_2h r b_dist_mm
                                   has_same_depth_prev has_same_depth_next cw_b cw_x1 cw_x2 cx_prev cx_next row_is_2h
                                   c_upg upg_spec corner_x corner_grnd_y prev_ref_cy next_ref_cy
                                   drawn_wale_tracker opp_hits opp_entry opp_seg_id opp_ray_xs opp_sp opp_c1_x opp_c2_x
                                   opp_seg_obj opp_sup_lst has_own_strut_at_depth opp_wale_sp opp_wale_b opp_is_2h_wale
                                   wale_already_drawn p_xs_in_opp xs_to_draw
                                   hrz_y ref_pair top_el_val proj_name sc_ent
                                   letter_str sec_char_code sec_pair_done section_pair_list
                                   plan_res pt1_plan in_norm1 opp_hit opp_ref_cy opp_x pt2_plan
                                   sec_mark_ents s_factor_sec top_box_y bot_pile_y cur_seg_data cur_emb_m)
  (vl-load-com)

  ;; 지보재 블록 5종 자동 등록
  (drg-ensure-support-blocks)

  ;; 0. TSP 세그먼트 데이터 확보
  (setq seg-list in-seg-list)
  (setq bnd-ent (if in-bnd-ent in-bnd-ent (if (boundp '*tsp-boundary-ent*) *tsp-boundary-ent* nil)))
  (setq bnd-orient
    (cond
      (in-bnd-orient in-bnd-orient)
      ((and (boundp '*tsp-boundary-orient*) *tsp-boundary-orient*) *tsp-boundary-orient*)
      ((and (boundp '*tsp-imported-orient*) *tsp-imported-orient*) *tsp-imported-orient*)
      (t -1)
    )
  )

  (if (null seg-list)
    (if (and (boundp '*segment-list*) *segment-list*
             (vl-some '(lambda (s) (cdr (assoc 'IS-DEFINED s))) *segment-list*))
      (setq seg-list *segment-list*)
    )
  )

  (if (null seg-list)
    (if (and (boundp 'tsp-get-project-list) (setq saved-projs (tsp-get-project-list)))
      (progn
        (setq dcl-path (if (boundp 'create-tsp-dcl) (create-tsp-dcl) nil))
        (if (and dcl-path (boundp 'project-list-callback))
          (if (project-list-callback dcl-path)
            (setq seg-list *segment-list*)
          )
        )
      )
    )
  )

  (if (null seg-list)
    (if (boundp 'tsp-parse-csv-file-data)
      (progn
        (setq csv-f (getfiled "전개도에 적용할 TSP CSV 파일 선택" "" "csv" 0))
        (if csv-f
          (progn
            (setq seg-list (tsp-parse-csv-file-data csv-f))
            (setq *segment-list* seg-list)
          )
        )
      )
    )
  )

  (if (null seg-list)
    (progn
      (alert "연동할 TSP 세그먼트 데이터를 찾을 수 없습니다.\n먼저 파이썬 GUI 또는 TSP에서 프로젝트를 로드하거나 생성해주세요.")
      (exit)
    )
  )

  (defun create_layer (lyr col / ldata)
    (if (not (tblsearch "LAYER" lyr))
      (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") (cons 2 lyr) '(70 . 0) (cons 62 col)))
      (progn
        (setq ldata (entget (tblobjname "LAYER" lyr)))
        (if (and ldata (/= (cdr (assoc 62 ldata)) col))
          (entmod (subst (cons 62 col) (assoc 62 ldata) ldata))
        )
      )
    )
  )

  (defun get-y-on-pline (pline_ent target_x / pts k end_p p1 p2 y_val min_x max_x pline_data)
    (setq pts nil pline_data (entget pline_ent))
    (if (= (cdr (assoc 0 pline_data)) "LWPOLYLINE")
      (progn
        (setq k 0.0 end_p (vlax-curve-getEndParam pline_ent))
        (while (<= k end_p)
          (setq pts (append pts (list (vlax-curve-getPointAtParam pline_ent k))))
          (setq k (1+ k))
        )
      )
      (if (= (cdr (assoc 0 pline_data)) "LINE")
        (setq pts (list (cdr (assoc 10 pline_data)) (cdr (assoc 11 pline_data))))
      )
    )
    (setq y_val nil k 0)
    (while (and (< k (1- (length pts))) (null y_val))
      (setq p1 (nth k pts) p2 (nth (1+ k) pts))
      (setq min_x (min (car p1) (car p2)) max_x (max (car p1) (car p2)))
      (if (and (>= target_x (- min_x 1.0)) (<= target_x (+ max_x 1.0)))
        (if (< (abs (- (car p2) (car p1))) 1e-4)
          (setq y_val (max (cadr p1) (cadr p2)))
          (setq y_val (+ (cadr p1) (* (- (cadr p2) (cadr p1)) (/ (- target_x (car p1)) (- (car p2) (car p1))))))
        )
      )
      (setq k (1+ k))
    )
    y_val
  )

  (defun get-max-y-on-pline (pline_ent / k end_p p max_y pline_data)
    (setq max_y -1e99 pline_data (entget pline_ent))
    (if (= (cdr (assoc 0 pline_data)) "LWPOLYLINE")
      (progn
        (setq k 0.0 end_p (vlax-curve-getEndParam pline_ent))
        (while (<= k end_p)
          (setq p (vlax-curve-getPointAtParam pline_ent k))
          (if (and p (> (cadr p) max_y)) (setq max_y (cadr p)))
          (setq k (1+ k))
        )
      )
      (if (= (cdr (assoc 0 pline_data)) "LINE")
        (setq max_y (max (cadr (cdr (assoc 10 pline_data))) (cadr (cdr (assoc 11 pline_data)))))
      )
    )
    max_y
  )

  (defun draw-capsule-shape (center-pt ang-dir ang-perp h-half w-half rad l-name col / p-c1 p-c2 p-c3 p-c4 p-c5 p-c6 p-c7 p-c8)
    (setq p-c1 (polar (polar center-pt ang-perp (- w-half rad)) ang-dir h-half))
    (setq p-c2 (polar (polar center-pt ang-perp w-half) ang-dir (- h-half rad)))
    (setq p-c3 (polar (polar center-pt ang-perp w-half) (+ ang-dir pi) (- h-half rad)))
    (setq p-c4 (polar (polar center-pt ang-perp (- w-half rad)) (+ ang-dir pi) h-half))
    (setq p-c5 (polar (polar center-pt (- ang-perp pi) (- w-half rad)) (+ ang-dir pi) h-half))
    (setq p-c6 (polar (polar center-pt (- ang-perp pi) w-half) (+ ang-dir pi) (- h-half rad)))
    (setq p-c7 (polar (polar center-pt (- ang-perp pi) w-half) ang-dir (- h-half rad)))
    (setq p-c8 (polar (polar center-pt (- ang-perp pi) (- w-half rad)) ang-dir h-half))
    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                   (cons 8 l-name) (cons 62 col) '(90 . 8) '(70 . 1)
                   (cons 10 (list (car p-c1) (cadr p-c1))) '(42 . -0.41421356) (cons 10 (list (car p-c2) (cadr p-c2))) '(42 . 0.0)
                   (cons 10 (list (car p-c3) (cadr p-c3))) '(42 . -0.41421356) (cons 10 (list (car p-c4) (cadr p-c4))) '(42 . 0.0)
                   (cons 10 (list (car p-c5) (cadr p-c5))) '(42 . -0.41421356) (cons 10 (list (car p-c6) (cadr p-c6))) '(42 . 0.0)
                   (cons 10 (list (car p-c7) (cadr p-c7))) '(42 . -0.41421356) (cons 10 (list (car p-c8) (cadr p-c8))) '(42 . 0.0)))
  )

  ;; 1. 종단면도 기준선 3개(_GRND, _굴착바닥고, _HRZ) 선택 (기존 2단계 _joint 원 선택 완전 생략)
  (princ "\n[TSP 전개도 자동작도] 종단면도 기준선 3개(_GRND, _굴착바닥고, _HRZ)를 선택하세요: ")
  (setq ss_lines (ssget '((0 . "LINE,LWPOLYLINE"))))
  (if (null ss_lines)
    (progn (alert "선택된 기준선 객체가 없습니다.") (exit))
  )

  (setq raw_list nil i 0)
  (while (< i (sslength ss_lines))
    (setq raw_list (append raw_list (list (ssname ss_lines i))))
    (setq i (1+ i))
  )

  (setq unique_list nil deleted_count 0)
  (foreach ent raw_list
    (setq p_start (vlax-curve-getStartPoint ent) p_end (vlax-curve-getEndPoint ent))
    (setq is_dup nil)
    (foreach u_ent unique_list
      (setq u_start (vlax-curve-getStartPoint u_ent) u_end (vlax-curve-getEndPoint u_ent))
      (if (or (and (< (distance p_start u_start) 1e-3) (< (distance p_end u_end) 1e-3))
              (and (< (distance p_start u_end) 1e-3) (< (distance p_end u_start) 1e-3)))
        (setq is_dup T)
      )
    )
    (if is_dup
      (progn (entdel ent) (setq deleted_count (1+ deleted_count)))
      (setq unique_list (append unique_list (list ent)))
    )
  )

  (if (/= (length unique_list) 3)
    (progn
      (alert (strcat "기준선은 중복 제외 정확히 3개여야 합니다. (현재 유효한 선: " (itoa (length unique_list)) "개)"))
      (exit)
    )
  )

  (setq line_list nil)
  (foreach ent unique_list
    (setq p_start (vlax-curve-getStartPoint ent) p_end (vlax-curve-getEndPoint ent))
    (setq avg_y (/ (+ (cadr p_start) (cadr p_end)) 2.0))
    (setq line_list (append line_list (list (list avg_y ent))))
  )
  (setq line_list (vl-sort line_list '(lambda (a b) (> (car a) (car b)))))
  (setq ent_grnd (cadr (nth 0 line_list)))
  (setq ent_excav (cadr (nth 1 line_list)))
  (setq ent_hrz (cadr (nth 2 line_list)))

  (setq hrz_p1 (vlax-curve-getStartPoint ent_hrz) hrz_p2 (vlax-curve-getEndPoint ent_hrz))
  (setq prof_start_x (min (car hrz_p1) (car hrz_p2)))
  (setq hrz_len (abs (- (car hrz_p2) (car hrz_p1))))
  (setq hrz_y (/ (+ (cadr hrz_p1) (cadr hrz_p2)) 2.0))
  (setq final_target_x (+ prof_start_x hrz_len))

  ;; 2. 측면말뚝 및 지보재 배치 방식 선택: [평면도 객체 투영(P) / 세그먼트 CTC 자동배치(C)]
  (initget "P C")
  (setq place_opt (getkword "\n측면말뚝 및 지보재 배치 방식 선택 [평면도 객체 투영(P) / 세그먼트 CTC 자동배치(C)] <P>: "))
  (if (null place_opt) (setq place_opt "P"))

  (setq ss_plan nil is_closed nil raw_piles_info nil raw_grouts_info nil plan_capsules nil)
  (setq proj_strut_xs nil proj_corner_xs nil proj_anchor_xs nil)

  (setq v0 (cdr (assoc 'V-START (nth 0 seg-list))))
  (setq v_last (cdr (assoc 'V-END (last seg-list))))
  (if (and v0 v_last (< (distance (list (car v0) (cadr v0)) (list (car v_last) (cadr v_last))) 10.0))
    (setq is_closed T)
  )

  (if (= place_opt "P")
    (progn
      (princ "\n[평면도 투영 모드] 평면도 전체(측면말뚝, 그라우팅, 지시객체, _mpoint, 버팀보, 사보강재, 앵커 포함)를 드래그하여 선택하세요: ")
      (setq ss_plan (ssget '((8 . "_지시,_측면말뚝,_그라우팅,*그라우팅*,*mpoint*,*버팀보*,*사보강재*,*앵커*"))))
      (if (null ss_plan)
        (progn
          (princ "\n-> 선택된 평면 객체가 없어 [세그먼트 CTC 자동배치(C)] 모드로 전환합니다.")
          (setq place_opt "C")
        )
      )
    )
  )

  (setq ent_bound nil)
  (if (and bnd-ent (entget bnd-ent))
    (progn
      (setq ent_bound bnd-ent)
      (princ "\n-> TSP 평면도 경계선 및 시점/진행방향을 자동으로 인식했습니다.")
    )
    (if (and (= place_opt "P") ss_plan)
      (progn
        (setq ent_bound_sel nil)
        (while (null ent_bound_sel)
          (setq ent_bound_sel (entsel "\n평면도 경계선을 선택하세요: "))
          (if (null ent_bound_sel) (princ "\n경계선이 선택되지 않았습니다. 다시 선택해주세요."))
        )
        (setq ent_bound (car ent_bound_sel))
      )
    )
  )

  (if ent_bound
    (progn
      (if (vlax-ldata-get ent_bound "TSP_ORIENT")
        (setq bnd-orient (vlax-ldata-get ent_bound "TSP_ORIENT"))
      )
      (setq total_bound_dist (vlax-curve-getDistAtParam ent_bound (vlax-curve-getEndParam ent_bound)))
      (setq is_closed (vlax-curve-isClosed ent_bound))
      (setq v0 (cdr (assoc 'V-START (nth 0 seg-list))))
      (setq v1_pt (cdr (assoc 'V-END (nth 0 seg-list))))
      (setq tsp_start_pt (vlax-ldata-get ent_bound "TSP_START_PT"))
      (if (null v0) (setq v0 tsp_start_pt))

      (if v0
        (progn
          (setq base_start_param (vlax-curve-getParamAtPoint ent_bound (vlax-curve-getClosestPointTo ent_bound v0)))
          (setq base_start_dist (vlax-curve-getDistAtParam ent_bound base_start_param))
          (if v1_pt
            (progn
              (setq dir_param (vlax-curve-getParamAtPoint ent_bound (vlax-curve-getClosestPointTo ent_bound v1_pt)))
              (setq dir_dist (vlax-curve-getDistAtParam ent_bound dir_param))
              (if is_closed
                (progn
                  (setq dist_diff (- dir_dist base_start_dist))
                  (if (< dist_diff 0) (setq dist_diff (+ total_bound_dist dist_diff)))
                  (setq is_forward (< dist_diff (/ total_bound_dist 2.0)))
                )
                (setq is_forward (>= dir_dist base_start_dist))
              )
            )
            (setq is_forward T)
          )
        )
        (setq base_start_dist 0.0 is_forward T)
      )
    )
  )

  (defun calc-pt-chain-on-bound (pt / cpt cur_d ch)
    (if (and ent_bound pt)
      (progn
        (setq cpt (vlax-curve-getClosestPointTo ent_bound (list (car pt) (cadr pt) 0.0)))
        (setq cur_d (vlax-curve-getDistAtPoint ent_bound cpt))
        (if is_closed
          (progn
            (if is_forward
              (setq ch (- cur_d base_start_dist))
              (setq ch (- base_start_dist cur_d))
            )
            (if (< (abs ch) 0.01) (setq ch 0.0))
            (if (< ch -0.01) (setq ch (+ total_bound_dist ch)))
          )
          (progn
            (setq ch cur_d)
            (if (not is_forward) (setq ch (- total_bound_dist ch)))
          )
        )
        ch
      )
      0.0
    )
  )

  ;; 3. TSP 모서리 좌표(seg-list의 V-START / 마지막 V-END) 기준 전개도 X좌표 리스트(corner_x_list) 직접 산출
  (setq corner_x_list nil corner_info_list nil)
  (setq num_segs (length seg-list))
  (if ent_bound
    (progn
      (setq seg_idx 0 prev_chain 0.0)
      (while (< seg_idx num_segs)
        (setq cur_seg (nth seg_idx seg-list))
        (setq v_pt (cdr (assoc 'V-START cur_seg)))
        (if (= seg_idx 0)
          (setq chain 0.0)
          (progn
            (setq chain (calc-pt-chain-on-bound v_pt))
            (if (< chain prev_chain)
              (setq chain (+ prev_chain (float (cdr (assoc 'LENGTH (nth (1- seg_idx) seg-list))))))
            )
          )
        )
        (setq prev_chain chain)
        (setq c_x (+ prof_start_x chain))
        (setq corner_x_list (append corner_x_list (list c_x)))
        (setq corner_info_list (append corner_info_list (list (list (1+ seg_idx) c_x))))
        (setq seg_idx (1+ seg_idx))
      )
      (if is_closed
        (progn
          (setq c_x (+ prof_start_x total_bound_dist))
          (setq corner_x_list (append corner_x_list (list c_x)))
          (setq corner_info_list (append corner_info_list (list (list 1 c_x))))
        )
        (progn
          (setq v_pt (cdr (assoc 'V-END (last seg-list))))
          (setq chain (calc-pt-chain-on-bound v_pt))
          (if (< chain prev_chain)
            (setq chain (+ prev_chain (float (cdr (assoc 'LENGTH (last seg-list))))))
          )
          (setq c_x (+ prof_start_x chain))
          (setq corner_x_list (append corner_x_list (list c_x)))
          (setq corner_info_list (append corner_info_list (list (list (1+ num_segs) c_x))))
        )
      )
    )
    (progn
      (setq total_seg_len 0.0)
      (foreach seg seg-list
        (setq total_seg_len (+ total_seg_len (float (cdr (assoc 'LENGTH seg)))))
      )
      (setq scale_len (if (and (> total_seg_len 1.0) (> hrz_len 1.0)) (/ hrz_len total_seg_len) 1.0))
      (setq cum_len 0.0 seg_idx 0)
      (while (< seg_idx num_segs)
        (setq c_x (+ prof_start_x (* cum_len scale_len)))
        (setq corner_x_list (append corner_x_list (list c_x)))
        (setq corner_info_list (append corner_info_list (list (list (1+ seg_idx) c_x))))
        (setq cum_len (+ cum_len (float (cdr (assoc 'LENGTH (nth seg_idx seg-list))))))
        (setq seg_idx (1+ seg_idx))
      )
      (setq c_x (+ prof_start_x (* cum_len scale_len)))
      (setq corner_x_list (append corner_x_list (list c_x)))
      (setq corner_info_list (append corner_info_list (list (list (if is_closed 1 (1+ num_segs)) c_x))))
    )
  )
  (setq final_target_x (last corner_x_list))

  ;; 3-1. P 모드일 때 평면도 객체(지보재, 캡슐, 측면말뚝) 추출
  (if (and (= place_opt "P") ss_plan ent_bound)
    (progn
      (setq proj_sup_data (drg-collect-projected-supports ss_plan ent_bound is_closed is_forward base_start_dist total_bound_dist prof_start_x))
      (setq proj_strut_xs  (nth 0 proj_sup_data)
            proj_corner_xs (nth 1 proj_sup_data)
            proj_anchor_xs (nth 2 proj_sup_data))

      (setq plan_capsules nil k 0 plan_circles nil plan_texts nil plan_lines nil plan_polys nil)
      (while (< k (sslength ss_plan))
        (setq p_ent (ssname ss_plan k) p_data (entget p_ent) p_type (cdr (assoc 0 p_data)) p_layer (cdr (assoc 8 p_data)))
        (cond
          ((and (= p_type "CIRCLE") (wcmatch (strcase p_layer) "_지시")) (setq plan_circles (append plan_circles (list p_data))))
          ((and (wcmatch p_type "*TEXT") (wcmatch (strcase p_layer) "_지시")) (setq plan_texts (append plan_texts (list p_data))))
          ((and (= p_type "LINE") (wcmatch (strcase p_layer) "_지시")) (setq plan_lines (append plan_lines (list p_ent))))
          ((and (= p_type "LWPOLYLINE") (wcmatch (strcase p_layer) "_지시")) (setq plan_polys (append plan_polys (list p_ent))))
        )
        (setq k (1+ k))
      )

      (defun is-pt-in-poly (pt poly_ent / data pts count i p1 p2 intersect_x)
        (setq data (entget poly_ent) pts nil)
        (foreach item data (if (= (car item) 10) (setq pts (append pts (list (cdr item))))))
        (setq count 0 i 0)
        (while (< i (length pts))
          (setq p1 (nth i pts))
          (if (= i (1- (length pts))) (setq p2 (nth 0 pts)) (setq p2 (nth (1+ i) pts)))
          (if (or (and (<= (cadr p1) (cadr pt)) (> (cadr p2) (cadr pt)))
                  (and (<= (cadr p2) (cadr pt)) (> (cadr p1) (cadr pt))))
            (progn
              (setq intersect_x (+ (car p1) (* (/ (- (cadr pt) (cadr p1)) (- (cadr p2) (cadr p1))) (- (car p2) (car p1)))))
              (if (< (car pt) intersect_x) (setq count (1+ count)))
            )
          )
          (setq i (1+ i))
        )
        (= (rem count 2) 1)
      )

      (setq poly_text_alist nil)
      (foreach poly_ent plan_polys
        (setq found_txt nil)
        (foreach t_data plan_texts
          (setq a72 (cdr (assoc 72 t_data)) a73 (cdr (assoc 73 t_data)))
          (setq t_pt (if (and a72 a73 (not (and (= a72 0) (= a73 0)))) (cdr (assoc 11 t_data)) (cdr (assoc 10 t_data))))
          (if (is-pt-in-poly t_pt poly_ent)
            (setq found_txt (cdr (assoc 1 t_data)))
          )
        )
        (if found_txt (setq poly_text_alist (append poly_text_alist (list (cons poly_ent found_txt)))))
      )

      (foreach c_data plan_circles
        (setq c_center (cdr (assoc 10 c_data)) c_rad (cdr (assoc 40 c_data)))
        (setq c_num 0 min_in 1e9)
        (foreach t_data plan_texts
          (setq a72 (cdr (assoc 72 t_data)) a73 (cdr (assoc 73 t_data)))
          (setq t_pt (if (and a72 a73 (not (and (= a72 0) (= a73 0)))) (cdr (assoc 11 t_data)) (cdr (assoc 10 t_data))))
          (setq dist (distance (list (car c_center) (cadr c_center)) (list (car t_pt) (cadr t_pt))))
          (if (and (<= dist (+ c_rad 1.0)) (< dist min_in))
            (progn (setq min_in dist c_num (atoi (cdr (assoc 1 t_data)))))
          )
        )
        (if (> c_num 0)
          (progn
            (setq cap_list nil connected_polys nil)
            (setq active_pts (list (list (car c_center) (cadr c_center))))
            (setq changed T avail_lines plan_lines)
            (while changed
              (setq changed nil remain_lines nil)
              (foreach l_ent avail_lines
                (setq p_start (vlax-curve-getStartPoint l_ent) p_end (vlax-curve-getEndPoint l_ent))
                (setq p1 (list (car p_start) (cadr p_start)) p2 (list (car p_end) (cadr p_end)))
                (setq is_connected nil)
                (foreach apt active_pts
                  (if (equal apt (list (car c_center) (cadr c_center)))
                    (if (or (< (abs (- (distance apt p1) c_rad)) 10.0)
                            (< (abs (- (distance apt p2) c_rad)) 10.0))
                      (setq is_connected T)
                    )
                    (if (or (< (distance apt p1) 5.0) (< (distance apt p2) 5.0))
                      (setq is_connected T)
                    )
                  )
                )
                (if is_connected
                  (progn
                    (setq active_pts (append active_pts (list p1 p2)) changed T)
                    (foreach poly_ent plan_polys
                      (if (not (member poly_ent connected_polys))
                        (progn
                          (setq cpt1 (vlax-curve-getClosestPointTo poly_ent p1))
                          (if (< (distance p1 (list (car cpt1) (cadr cpt1))) 5.0)
                            (progn (setq connected_polys (append connected_polys (list poly_ent))) (setq active_pts (append active_pts (list (list (car cpt1) (cadr cpt1))))))
                          )
                          (setq cpt2 (vlax-curve-getClosestPointTo poly_ent p2))
                          (if (< (distance p2 (list (car cpt2) (cadr cpt2))) 5.0)
                            (progn (setq connected_polys (append connected_polys (list poly_ent))) (setq active_pts (append active_pts (list (list (car cpt2) (cadr cpt2))))))
                          )
                        )
                      )
                    )
                  )
                  (setq remain_lines (append remain_lines (list l_ent)))
                )
              )
              (setq avail_lines remain_lines)
            )
            (foreach poly_ent connected_polys
              (setq txt (cdr (assoc poly_ent poly_text_alist)))
              (if txt (setq cap_list (append cap_list (list txt))))
            )
            (if cap_list
              (progn
                (setq cap_list (vl-sort cap_list '(lambda (a b) (< (atoi a) (atoi b)))))
                (setq plan_capsules (append plan_capsules (list (list c_num cap_list))))
              )
            )
          )
        )
      )

      (defun get-pile-size-from-name (blk_name / upper_name)
        (setq upper_name (strcase blk_name))
        (cond
          ((vl-string-search "250" upper_name) 250.0)
          ((or (vl-string-search "298" upper_name) (vl-string-search "201" upper_name)) 201.0)
          ((vl-string-search "300" upper_name) 300.0)
          ((vl-string-search "350" upper_name) 350.0)
          ((vl-string-search "400" upper_name) 400.0)
          (t nil)
        )
      )

      (setq raw_piles_info nil raw_grouts_info nil k 0)
      (while (< k (sslength ss_plan))
        (setq p_ent (ssname ss_plan k) p_data (entget p_ent))
        (cond
          ;; 1) 평면도 측면말뚝(INSERT) 투영
          ((and (= (cdr (assoc 0 p_data)) "INSERT")
                (wcmatch (strcase (cdr (assoc 8 p_data))) "*측면말뚝*"))
           (setq blk_name (cdr (assoc 2 p_data)))
           (setq ext_size (if blk_name (get-pile-size-from-name blk_name) nil))
           (setq ins_pt (cdr (assoc 10 p_data)))
           (setq cpt (vlax-curve-getClosestPointTo ent_bound (list (car ins_pt) (cadr ins_pt) 0.0)))
           (if cpt
             (progn
               (if is_closed
                 (progn
                   (setq cur_dist (vlax-curve-getDistAtPoint ent_bound cpt))
                   (if is_forward
                     (setq chain (- cur_dist base_start_dist))
                     (setq chain (- base_start_dist cur_dist))
                   )
                   (if (< chain 0) (setq chain (+ total_bound_dist chain)))
                 )
                 (progn
                   (setq chain (vlax-curve-getDistAtPoint ent_bound cpt))
                   (if (not is_forward) (setq chain (- total_bound_dist chain)))
                 )
               )
               (setq raw_piles_info (append raw_piles_info (list (list (+ prof_start_x chain) ext_size (list (car ins_pt) (cadr ins_pt) 0.0)))))
             )
           )
          )
          ;; 2) 평면도 차수그라우팅(CIRCLE / ARC) 중심점 -> 경계선 수선 투영 및 직경(D = 2*R) 추출
          ((and (member (cdr (assoc 0 p_data)) '("CIRCLE" "ARC"))
                (wcmatch (strcase (cdr (assoc 8 p_data))) "*그라우팅*"))
           (setq g_rad (float (cdr (assoc 40 p_data))))
           ;; D600 내부 코어 표시용 작은 원(반지름 25mm)은 제외하고 실제 그라우팅 외곽 원/호(반지름 50mm 이상)만 추출
           (if (> g_rad 50.0)
             (progn
               (setq g_cen (cdr (assoc 10 p_data)))
               (setq g_dia (* g_rad 2.0))
               (setq cpt (vlax-curve-getClosestPointTo ent_bound (list (car g_cen) (cadr g_cen) 0.0)))
               (if cpt
                 (progn
                   (if is_closed
                     (progn
                       (setq cur_dist (vlax-curve-getDistAtPoint ent_bound cpt))
                       (if is_forward
                         (setq chain (- cur_dist base_start_dist))
                         (setq chain (- base_start_dist cur_dist))
                       )
                       (if (< chain 0) (setq chain (+ total_bound_dist chain)))
                     )
                     (progn
                       (setq chain (vlax-curve-getDistAtPoint ent_bound cpt))
                       (if (not is_forward) (setq chain (- total_bound_dist chain)))
                     )
                   )
                   (setq g_x (+ prof_start_x chain))
                   ;; 동일 그라우팅이 다중 ARC로 분할된 경우 중복 추가 방지
                   (if (not (vl-some '(lambda (item) (and (< (abs (- (car item) g_x)) 2.0) (< (abs (- (cadr item) g_dia)) 2.0))) raw_grouts_info))
                     (setq raw_grouts_info (append raw_grouts_info (list (list g_x g_dia))))
                   )
                 )
               )
             )
           )
          )
        )
        (setq k (1+ k))
      )
      (setq raw_piles_info (vl-sort raw_piles_info '(lambda (a b) (< (car a) (car b)))))
      (if raw_grouts_info
        (setq raw_grouts_info (vl-sort raw_grouts_info '(lambda (a b) (< (car a) (car b)))))
      )
    )
  )

  ;; 4. TSP 모서리 X좌표(corner_x_list) 및 SUB-ZONES 기반 계단식 스팬 생성
  (setq tsp_spans (drg-build-tsp-elevation-spans seg-list corner_x_list prof_start_x))

  ;; 4-0. 세그먼트 스팬 매핑 테이블(seg_span_map) 사전 구축
  (setq seg_span_map nil)
  (foreach sp tsp_spans
    (setq seg_id (nth 5 sp) seg_c1_x (nth 13 sp) seg_c2_x (nth 14 sp) scale_f (nth 15 sp))
    (if (not (assoc seg_id seg_span_map))
      (setq seg_span_map (append seg_span_map (list (list seg_id seg_c1_x seg_c2_x scale_f))))
    )
  )

  ;; 4-0-A. 사용자가 단면 위치를 찍기 전에 각 세그먼트 경계를 눈으로 확인할 수 있도록
  ;;        전개도 상단 모서리 지시선(_지시)을 선행 생성하기 위해 측면말뚝 좌표를 사전 계산
  (setq pre_piles_x nil)
  (foreach sp tsp_spans
    (setq sec_start_x (nth 0 sp) sec_end_x (nth 1 sp) ctc (nth 4 sp))
    (if (and (= place_opt "P") raw_piles_info)
      (foreach p_info raw_piles_info
        (setq px (car p_info))
        (if (and (>= px (- sec_start_x 1.0)) (<= px (+ sec_end_x 1.0)))
          (if (not (vl-some '(lambda (x) (< (abs (- x px)) 1.0)) pre_piles_x))
            (setq pre_piles_x (append pre_piles_x (list px)))
          )
        )
      )
      (progn
        (setq curr_x sec_start_x)
        (while (< curr_x (- sec_end_x 1.0))
          (if (not (vl-some '(lambda (x) (< (abs (- x curr_x)) 1.0)) pre_piles_x))
            (setq pre_piles_x (append pre_piles_x (list curr_x)))
          )
          (setq curr_x (+ curr_x ctc))
        )
        (if (not (vl-some '(lambda (x) (< (abs (- x sec_end_x)) 1.0)) pre_piles_x))
          (setq pre_piles_x (append pre_piles_x (list sec_end_x)))
        )
      )
    )
  )
  (setq pre_piles_x (vl-sort pre_piles_x '<))
  ;; 폐합선인 경우 마지막 끝점(허수 말뚝)은 캡슐 번호 카운트에서 제외
  (if (and is_closed (> (length pre_piles_x) 1) (< (abs (- (last pre_piles_x) final_target_x)) 1.0))
    (setq pre_piles_x (reverse (cdr (reverse pre_piles_x))))
  )

  (defun find-pile-idx-near-x-pre (target_x px_list / idx best_idx min_d d)
    (setq idx 1 best_idx nil min_d 1e99)
    (foreach xval px_list
      (setq d (abs (- xval target_x)))
      (if (< d min_d)
        (setq min_d d best_idx idx)
      )
      (setq idx (1+ idx))
    )
    best_idx
  )

  (setq max_grnd_y (get-max-y-on-pline ent_grnd))
  (setq def_txt_hgt (if (vlax-ldata-get "DRG_SETTINGS" "TXT_HGT") (vlax-ldata-get "DRG_SETTINGS" "TXT_HGT") 600.0))
  (setq def_joint_rad (* def_txt_hgt 1.5))
  (setq max_joint_top_y (+ max_grnd_y 6000.0 (* def_joint_rad 2.0)))

  (setq ss_exist_joints (ssget "X" '((0 . "CIRCLE") (8 . "_joint,_바닥번호"))))
  (if ss_exist_joints
    (progn
      (setq k 0)
      (while (< k (sslength ss_exist_joints))
        (setq j_ent (ssname ss_exist_joints k) j_data (entget j_ent))
        (setq j_cen (cdr (assoc 10 j_data)) j_rad (cdr (assoc 40 j_data)))
        (if (and (>= (car j_cen) (- prof_start_x 5000.0))
                 (<= (car j_cen) (+ final_target_x 5000.0))
                 (<= (cadr j_cen) (+ max_grnd_y 30000.0)))
          (if (> (+ (cadr j_cen) j_rad) max_joint_top_y)
            (setq max_joint_top_y (+ (cadr j_cen) j_rad))
          )
        )
        (setq k (1+ k))
      )
    )
  )

  ;; 전개도 상단 모서리 지시선(_지시) 선행 작도 및 화면 즉시 갱신
  (create_layer "_지시" 1)
  (setq item_idx 0 total_c (length corner_info_list))
  (foreach c_item corner_info_list
    (setq e_num (nth 0 c_item) e_x (nth 1 c_item))
    (setq match_caps nil)
    (foreach pc plan_capsules
      (if (= (car pc) e_num) (setq match_caps (cadr pc)))
    )

    (if match_caps
      (progn
        (setq caps_to_draw match_caps)
        (if (and is_closed (= e_num 1) (= item_idx 0))
          (setq caps_to_draw (list (car match_caps)))
        )
      )
      (progn
        (setq caps_to_draw nil)
        (if (> (length pre_piles_x) 0)
          (cond
            ((= item_idx 0)
             (setq caps_to_draw (list "1"))
            )
            ((and is_closed (= item_idx (1- total_c)))
             (setq last_p_idx (length pre_piles_x))
             (if (> last_p_idx 1)
               (setq caps_to_draw (list "1" (itoa last_p_idx)))
               (setq caps_to_draw (list "1"))
             )
            )
            (t
             (setq near_idx (find-pile-idx-near-x-pre e_x pre_piles_x))
             (if near_idx
               (setq caps_to_draw (list (itoa near_idx)))
             )
            )
          )
        )
      )
    )

    (setq base_y max_joint_top_y cur_y base_y)
    (setq red_line_start (list e_x cur_y 0.0) cur_y (+ cur_y 1300.0) red_line_end (list e_x cur_y 0.0))
    (entmake (list '(0 . "LINE") '(8 . "_지시") '(62 . 1) (cons 10 red_line_start) (cons 11 red_line_end)))

    (if caps_to_draw
      (progn
        (setq rev_caps (reverse caps_to_draw) cap_idx 0 num_caps (length rev_caps) cap_h_half 600.0 cap_c_rad 480.0)
        (foreach cap_str rev_caps
          (setq cap_center_y (+ cur_y cap_h_half) cap_center (list e_x cap_center_y 0.0))
          (setq cap_w_half (+ cap_c_rad (* (strlen cap_str) 300.0)))
          (draw-capsule-shape cap_center (/ pi 2.0) 0.0 cap_h_half cap_w_half cap_c_rad "_지시" 1)
          (entmake (list '(0 . "TEXT") '(8 . "_지시") '(62 . 7) (cons 10 cap_center) (cons 11 cap_center)
                         '(40 . 720.0) (cons 50 0.0) (cons 1 cap_str)
                         '(7 . "Standard") '(72 . 1) '(73 . 2)))
          (setq cur_y (+ cur_y (* cap_h_half 2.0)))
          (if (< cap_idx (1- num_caps))
            (progn
              (setq red_short_start (list e_x cur_y 0.0) cur_y (+ cur_y 600.0) red_short_end (list e_x cur_y 0.0))
              (entmake (list '(0 . "LINE") '(8 . "_지시") '(62 . 1) (cons 10 red_short_start) (cons 11 red_short_end)))
            )
          )
          (setq cap_idx (1+ cap_idx))
        )
      )
    )

    (setq yel_line_start (list e_x cur_y 0.0) cur_y (+ cur_y 1300.0) yel_line_end (list e_x cur_y 0.0))
    (entmake (list '(0 . "LINE") '(8 . "_지시") '(62 . 2) (cons 10 yel_line_start) (cons 11 yel_line_end)))
    (setq top_circle_rad 1200.0 top_circle_center (list e_x (+ cur_y top_circle_rad) 0.0))
    (entmake (list '(0 . "CIRCLE") '(8 . "_지시") '(62 . 2) (cons 10 top_circle_center) (cons 40 top_circle_rad)))
    (entmake (list '(0 . "TEXT") '(8 . "_지시") '(62 . 7) (cons 10 top_circle_center) (cons 11 top_circle_center)
                   '(40 . 840.0) (cons 50 0.0) (cons 1 (itoa e_num))
                   '(7 . "Standard") '(72 . 1) '(73 . 2)))

    (setq item_idx (1+ item_idx))
  )
  (setq *drg-top-ind-pre-drawn* T)
  (vla-regen (vla-get-activedocument (vlax-get-acad-object)) acActiveViewport)
  (princ "\n-> 전개도 상단에 세그먼트 경계 지시선(_지시)을 먼저 생성했습니다. 지시선 구간을 참고하여 단면 위치를 지정하세요.")

    ;; 4-1 & 4-1-A 통합: 세그먼트별 기준 단면 지점 지정 및 스마트 오클릭/관통단면 자동 판별 (방식 B)
  ;;   - 클릭한 X좌표가 속한 세그먼트 번호를 반환하는 보조 함수
  (defun drg-get-seg-id-at-x (xval span-map / found-id item c1 c2)
    (setq found-id nil)
    (foreach item span-map
      (setq c1 (float (nth 1 item)) c2 (float (nth 2 item)))
      (if (and (null found-id) (>= xval (- c1 1.0)) (<= xval (+ c2 1.0)))
        (setq found-id (nth 0 item))
      )
    )
    found-id
  )

  ;;   - 클릭한 X좌표가 코너(모서리 지시선 X좌표)와 정확히 일치(1.0mm 이내)하는지 검사하여 코너 X좌표 반환
  (defun drg-find-exact-corner-x (xval c-x-list / matched-cx cx)
    (setq matched-cx nil)
    (foreach cx c-x-list
      (if (and (null matched-cx) (<= (abs (- xval (float cx))) 1.0))
        (setq matched-cx (float cx))
      )
    )
    matched-cx
  )

  ;;   - 코너 X좌표(cx)를 경계로 맞닿아 있는 (list 왼쪽세그먼트ID 오른쪽세그먼트ID) 반환
  (defun drg-get-adjacent-segs-at-corner (cx span-map is-closed-flag / left-id right-id item sid c1 c2 n-segs)
    (setq left-id nil right-id nil n-segs (length span-map))
    (foreach item span-map
      (setq sid (nth 0 item)
            c1  (float (nth 1 item))
            c2  (float (nth 2 item)))
      (if (<= (abs (- cx c2)) 1.0) (setq left-id sid))
      (if (<= (abs (- cx c1)) 1.0) (setq right-id sid))
    )
    ;; 폐합선에서 맨 왼쪽 시점 코너(1번) 또는 맨 오른쪽 종점 코너(1번)를 찍은 경우 순환 연결 지원
    (if (and is-closed-flag (> n-segs 1))
      (progn
        (if (and (null left-id) (= right-id 1)) (setq left-id n-segs))
        (if (and (null right-id) (= left-id n-segs)) (setq right-id 1))
      )
    )
    (list left-id right-id)
  )

  ;;   - 해당 세그먼트에 유효한 단면/지반/지보재 데이터가 부여되어 있는지 확인하는 보조 함수
  (defun drg-seg-has-own-section-data-p (s-id s-list / s-obj)
    (setq s-obj (if (and s-list (>= s-id 1) (<= s-id (length s-list))) (nth (1- s-id) s-list) nil))
    (and s-obj
         (or (cdr (assoc 'IS-DEFINED s-obj))
             (and (assoc 'SOIL-DATA s-obj) (cdr (assoc 'SOIL-DATA s-obj)))
             (and (assoc 'SUPPORT-LIST s-obj) (cdr (assoc 'SUPPORT-LIST s-obj)))
             (and (assoc 'SUB-ZONES s-obj) (cdr (assoc 'SUB-ZONES s-obj)))))
  )

  (setq seg_ref_cy_map nil seg_ref_pt_map nil)
  (setq section_pair_list nil sec_pair_done nil sec_char_code 65)

  (foreach sp_entry seg_span_map
    (setq seg_id (nth 0 sp_entry)
          seg_c1_x (float (nth 1 sp_entry))
          seg_c2_x (float (nth 2 sp_entry))
          scale_f  (float (nth 3 sp_entry)))
    (setq mid_x (/ (+ seg_c1_x seg_c2_x) 2.0))

    (cond
      ;; [Case 1] 앞선 세그먼트의 맞은편 관통단면으로 이미 자동 매칭 완료된 세그먼트(예: Seg-1과 묶인 Seg-3) -> 프롬프트 자동 생략!
      ((member seg_id sec_pair_done)
       (if (not (assoc seg_id seg_ref_cy_map))
         (progn
           (setq ref_cy (get-y-on-pline ent_grnd mid_x))
           (if (null ref_cy) (setq ref_cy (cadr (vlax-curve-getStartPoint ent_grnd))))
           (setq seg_ref_cy_map (append seg_ref_cy_map (list (cons seg_id ref_cy))))
         )
       )
       (princ (strcat "\n  -> [자동연동 완료] Seg-" (itoa seg_id) " 구간은 앞서 맞은편 관통단면으로 이미 위치가 확정되어 입력을 생략합니다."))
      )

      ;; [Case 2] 아직 매칭되지 않은 세그먼트 -> 사용자에게 단면 위치 클릭 요청 (데이터가 없더라도 클릭 시 단면 생성, Enter 시 미정의면 생략)
      (t
       (setq has_own_data (drg-seg-has-own-section-data-p seg_id seg-list))
       (setq valid_pick nil)
       (setq skip_this_seg nil)

       (while (not valid_pick)
         (setq next_corner_num (if (and is_closed (= seg_id (length seg-list))) 1 (1+ seg_id)))
         (setq prompt_msg
           (if has_own_data
             (strcat "\n[기준단면 지정] Seg-" (itoa seg_id)
                     " (상단 지시선 " (itoa seg_id) "번~" (itoa next_corner_num) "번 사이) 기준 지반고 위치를 클릭하세요 <Enter: 구간 중앙점 자동>: ")
             (strcat "\n[기준단면 지정] Seg-" (itoa seg_id)
                     " (상단 지시선 " (itoa seg_id) "번~" (itoa next_corner_num) "번 사이) 단면 위치를 클릭하세요 <Enter: 단면 생략>: ")
           )
         )
         (setq ref_pt (getpoint prompt_msg))

         (if (null ref_pt)
           ;; Enter 입력 시
           (if has_own_data
             (progn
               (setq ref_x mid_x)
               (setq ref_cy (get-y-on-pline ent_grnd ref_x))
               (if (null ref_cy) (setq ref_cy (cadr (vlax-curve-getStartPoint ent_grnd))))
               (setq seg_ref_cy_map (append seg_ref_cy_map (list (cons seg_id ref_cy))))
               (setq seg_ref_pt_map (append seg_ref_pt_map (list (list seg_id ref_x ref_cy))))
               (princ (strcat "\n  -> Seg-" (itoa seg_id) " 구간 중앙점(X=" (rtos ref_x 2 1) ", GL Y=" (rtos ref_cy 2 2) ")으로 확정했습니다."))
               (setq valid_pick T)
               (setq picked_cross_info nil)
             )
             (progn
               (setq ref_cy (get-y-on-pline ent_grnd mid_x))
               (if (null ref_cy) (setq ref_cy (cadr (vlax-curve-getStartPoint ent_grnd))))
               (setq seg_ref_cy_map (append seg_ref_cy_map (list (cons seg_id ref_cy))))
               (princ (strcat "\n  -> Seg-" (itoa seg_id) " 구간 단면 표시를 생략합니다."))
               (setq valid_pick T)
               (setq skip_this_seg T)
             )
           )
           ;; 사용자가 좌표를 클릭한 경우
           (progn
             (setq click_x (car ref_pt))
             ;; 클릭한 위치가 전개도 측면말뚝(H-Pile) 폭 내부이면 해당 H-Pile 중심 X좌표로 자동 스냅!
             (if (and (boundp 'raw_piles_info) raw_piles_info)
               (foreach p_info raw_piles_info
                 (if (<= (abs (- click_x (car p_info))) (+ (/ (if (cadr p_info) (cadr p_info) 300.0) 2.0) 10.0))
                   (setq click_x (car p_info))
                 )
               )
             )

             ;; ★ [코너 클릭 감지 및 2차 방향(왼쪽/오른쪽) 클릭 처리]
             (setq exact_corner_x (drg-find-exact-corner-x click_x corner_x_list))
             (setq chosen_seg_id nil)
             (setq dir_pick_cancelled nil)

             (if exact_corner_x
               (progn
                 (setq click_x exact_corner_x)
                 (setq adj_segs (drg-get-adjacent-segs-at-corner exact_corner_x seg_span_map is_closed))
                 (setq left_seg_id (nth 0 adj_segs)
                       right_seg_id (nth 1 adj_segs))
                 (setq dir_loop T)
                 (while dir_loop
                   (setq dir_pt
                     (getpoint (list exact_corner_x (cadr ref_pt) 0.0)
                       (strcat "\n  -> [코너 위치 선택됨] 적용할 벽체 방향(왼쪽"
                               (if left_seg_id (strcat ":Seg-" (itoa left_seg_id)) ":없음")
                               " / 오른쪽"
                               (if right_seg_id (strcat ":Seg-" (itoa right_seg_id)) ":없음")
                               ")을 한 번 더 클릭하세요: ")))
                   (if (null dir_pt)
                     (progn
                       (princ "\n  -> 방향 선택이 취소되었습니다. 단면 위치를 다시 클릭해주세요.")
                       (setq dir_pick_cancelled T dir_loop nil)
                     )
                     (progn
                       (setq dx_dir (- (car dir_pt) exact_corner_x))
                       (cond
                         ((< (abs dx_dir) 1.0)
                          (princ "\n  -> 코너와 동일한 X좌표입니다. 코너보다 왼쪽 또는 오른쪽을 명확히 클릭해주세요.")
                         )
                         ((< dx_dir 0.0)
                          (if left_seg_id
                            (progn
                              (setq chosen_seg_id left_seg_id dir_loop nil)
                              ;; 폐합선 맨 왼쪽(시점) 코너에서 왼쪽(마지막 세그먼트)을 선택한 경우 해당 세그먼트 끝 코너 X좌표로 매칭
                              (setq chosen_sp (assoc chosen_seg_id seg_span_map))
                              (if (and chosen_sp (> (abs (- click_x (float (nth 1 chosen_sp)))) 1.0) (> (abs (- click_x (float (nth 2 chosen_sp)))) 1.0))
                                (setq click_x (float (nth 2 chosen_sp)))
                              )
                              (princ (strcat "\n  -> 코너 왼쪽 방향 [Seg-" (itoa chosen_seg_id) "] 벽체가 선택되었습니다."))
                            )
                            (princ "\n  -> 해당 코너의 왼쪽에는 유효한 세그먼트가 없습니다. 오른쪽을 클릭해주세요.")
                          )
                         )
                         (t
                          (if right_seg_id
                            (progn
                              (setq chosen_seg_id right_seg_id dir_loop nil)
                              ;; 폐합선 맨 오른쪽(종점) 코너에서 오른쪽(첫 세그먼트)을 선택한 경우 첫 세그먼트 시작 코너 X좌표로 매칭
                              (setq chosen_sp (assoc chosen_seg_id seg_span_map))
                              (if (and chosen_sp (> (abs (- click_x (float (nth 1 chosen_sp)))) 1.0) (> (abs (- click_x (float (nth 2 chosen_sp)))) 1.0))
                                (setq click_x (float (nth 1 chosen_sp)))
                              )
                              (princ (strcat "\n  -> 코너 오른쪽 방향 [Seg-" (itoa chosen_seg_id) "] 벽체가 선택되었습니다."))
                            )
                            (princ "\n  -> 해당 코너의 오른쪽에는 유효한 세그먼트가 없습니다. 왼쪽을 클릭해주세요.")
                          )
                         )
                       )
                     )
                   )
                 )
               )
               ;; 코너가 아닌 일반 위치를 클릭한 경우
               (if (and (>= click_x (- seg_c1_x 1.0)) (<= click_x (+ seg_c2_x 1.0)))
                 (setq chosen_seg_id seg_id)
                 (setq chosen_seg_id (drg-get-seg-id-at-x click_x seg_span_map))
               )
             )

             (if (not dir_pick_cancelled)
               (if (= chosen_seg_id seg_id)
                 ;; (A) 현재 세그먼트(Seg-id)로 확정된 경우 (구간 내부 클릭 또는 코너 클릭 후 현재 세그먼트 방향 선택)
                 (progn
                   (setq ref_x (max seg_c1_x (min seg_c2_x click_x)))
                   (setq ref_cy (get-y-on-pline ent_grnd ref_x))
                   (if (null ref_cy) (setq ref_cy (cadr (vlax-curve-getStartPoint ent_grnd))))
                   (setq seg_ref_cy_map (append seg_ref_cy_map (list (cons seg_id ref_cy))))
                   (setq seg_ref_pt_map (append seg_ref_pt_map (list (list seg_id ref_x ref_cy))))
                   (princ (strcat "\n  -> Seg-" (itoa seg_id) " 기준 지반고(GL 0.0) Y좌표 확정: " (rtos ref_cy 2 2)))
                   (setq valid_pick T)
                   (setq picked_cross_info nil)
                 )
                 ;; (B) 다른 세그먼트를 선택한 경우 (범위 밖 클릭 또는 코너에서 다른 세그먼트 방향 선택) -> [1. 잘못 찍음] vs [2. 맞은편 관통단면] 자동 판별
                 (progn
                   (setq clicked_seg_id chosen_seg_id)
                 (setq cross_matched nil)

                 (if (and clicked_seg_id (/= clicked_seg_id seg_id))
                   (progn
                     ;; 1차 & 2차: 평면도 기하 광선/수선의 발 추적
                     (setq c_plan_res
                       (vl-catch-all-apply 'drg-elev-x-to-plan-pt
                         (list click_x clicked_seg_id seg-list seg_span_map ent_bound is_closed is_forward
                               base_start_dist total_bound_dist bnd-orient)))
                     (if (and c_plan_res (not (vl-catch-all-error-p c_plan_res)))
                       (progn
                         (setq c_pt_plan (nth 0 c_plan_res)
                               c_in_norm (nth 2 c_plan_res))
                         (setq c_opp_hit
                           (vl-catch-all-apply 'drg-find-opposite-section-hit
                             (list clicked_seg_id c_pt_plan c_in_norm seg-list seg_span_map ent_bound is_closed is_forward
                                   base_start_dist total_bound_dist bnd-orient)))
                         (if (and c_opp_hit (not (vl-catch-all-error-p c_opp_hit))
                                  (= (nth 0 c_opp_hit) seg_id))
                           (setq cross_matched (list clicked_seg_id click_x c_pt_plan c_in_norm (nth 1 c_opp_hit) (nth 2 c_opp_hit)))
                         )
                       )
                     )

                     ;; 3차 폴백: 만약 평면도 엔티티 누락 등으로 1·2차가 실패하더라도,
                     ;; ㄷ자/직사각형에서 마주보는 벽체 번호(|clicked_seg_id - seg_id| == 2)이면 전개도 대칭 위치로 즉시 매칭!
                     (if (and (null cross_matched) (= (abs (- clicked_seg_id seg_id)) 2))
                       (progn
                         (setq fb_hit
                           (drg-fallback-opposite-by-span
                             clicked_seg_id click_x seg-list seg_span_map ent_bound is_closed is_forward
                             base_start_dist total_bound_dist bnd-orient))
                         (if (and fb_hit (= (nth 0 fb_hit) seg_id))
                           (setq cross_matched
                             (list clicked_seg_id
                                   click_x
                                   (if (and c_plan_res (not (vl-catch-all-error-p c_plan_res))) (nth 0 c_plan_res) nil)
                                   (if (and c_plan_res (not (vl-catch-all-error-p c_plan_res))) (nth 2 c_plan_res) 0.0)
                                   (nth 1 fb_hit)
                                   (nth 2 fb_hit)))
                         )
                       )
                     )
                   )
                 )

                 (if cross_matched
                   ;; [판별 2: 맞은편 벽체까지의 관통단면을 원하는 것으로 자동 판별]
                   (progn
                     (setq opp_seg_id (nth 0 cross_matched)
                           opp_x      (nth 1 cross_matched)
                           pt2_plan   (nth 2 cross_matched)
                           pt1_plan   (nth 4 cross_matched)
                           ref_x      (nth 5 cross_matched))
                     (setq ref_x (max seg_c1_x (min seg_c2_x ref_x)))
                     (setq ref_cy (get-y-on-pline ent_grnd ref_x))
                     (if (null ref_cy) (setq ref_cy (cadr (vlax-curve-getStartPoint ent_grnd))))
                     (setq opp_ref_cy (get-y-on-pline ent_grnd opp_x))
                     (if (null opp_ref_cy) (setq opp_ref_cy ref_cy))

                     (setq plan_res_cur
                       (vl-catch-all-apply 'drg-elev-x-to-plan-pt
                         (list ref_x seg_id seg-list seg_span_map ent_bound is_closed is_forward
                               base_start_dist total_bound_dist bnd-orient)))
                     (if (and plan_res_cur (not (vl-catch-all-error-p plan_res_cur)))
                       (progn
                         (if (null pt1_plan) (setq pt1_plan (nth 0 plan_res_cur)))
                         (setq in_norm1 (nth 2 plan_res_cur))
                       )
                       (setq in_norm1 0.0)
                     )

                     (setq seg_ref_cy_map (append seg_ref_cy_map (list (cons seg_id ref_cy))))
                     (setq seg_ref_pt_map (append seg_ref_pt_map (list (list seg_id ref_x ref_cy))))
                     (if (not (assoc opp_seg_id seg_ref_cy_map))
                       (setq seg_ref_cy_map (append seg_ref_cy_map (list (cons opp_seg_id opp_ref_cy))))
                     )

                     (setq picked_cross_info (list opp_seg_id opp_x opp_ref_cy pt2_plan pt1_plan in_norm1))
                     (setq valid_pick T)
                     (princ (strcat "\n  -> [알림: 관통단면 자동판별] 클릭하신 Seg-" (itoa opp_seg_id)
                                    " 위치(X=" (rtos opp_x 2 1) ")는 현재 Seg-" (itoa seg_id)
                                    "의 맞은편 벽체입니다.\n     두 벽체를 가로지르는 관통단면으로 인식하여 Seg-" (itoa seg_id)
                                    " 대응 위치(X=" (rtos ref_x 2 1) ")와 Seg-" (itoa opp_seg_id)
                                    " 단면 위치를 동시에 확정했습니다."))
                   )
                                      ;; [판별 1: 잘못 찍음 (단순 오클릭) -> 경고 알림 후 재클릭 유도]
                   (progn
                     (if clicked_seg_id
                       (princ (strcat "\n  -> [주의: 잘못 클릭됨] 선택하신 방향은 인접한 [Seg-" (itoa clicked_seg_id)
                                      "] 구간이며, 현재 [Seg-" (itoa seg_id)
                                      "]의 맞은편 관통 벽체가 아닙니다.\n     상단 지시선 " (itoa seg_id) "번~" (itoa next_corner_num)
                                      "번 사이(X: " (rtos seg_c1_x 2 0) " ~ " (rtos seg_c2_x 2 0) ")를 다시 클릭해주세요."))
                       (princ (strcat "\n  -> [주의: 잘못 클릭됨] 클릭하신 위치가 전개도 범위를 벗어났습니다.\n     상단 지시선 "
                                      (itoa seg_id) "번~" (itoa next_corner_num)
                                      "번 사이(X: " (rtos seg_c1_x 2 0) " ~ " (rtos seg_c2_x 2 0) ")를 다시 클릭해주세요."))
                     )
                   )
                 )
                 )
               )
             )
           )
         )
       )

       ;; 생략(skip_this_seg)이 아닌 경우에만 단면기호 페어(section_pair_list) 등록
       (if (not skip_this_seg)
         (progn
           (setq letter_str (chr sec_char_code))
           (if picked_cross_info
             (progn
               ;; 범위 밖 맞은편 벽체를 직접 클릭하여 관통단면으로 판별된 경우
               (setq opp_seg_id (nth 0 picked_cross_info)
                     opp_x      (nth 1 picked_cross_info)
                     opp_ref_cy (nth 2 picked_cross_info)
                     pt2_plan   (nth 3 picked_cross_info)
                     pt1_plan   (nth 4 picked_cross_info)
                     in_norm1   (nth 5 picked_cross_info))
               (setq sec_pair_done (append sec_pair_done (list seg_id opp_seg_id)))
               (setq section_pair_list
                 (append section_pair_list
                   (list (list letter_str seg_id ref_x ref_cy pt1_plan in_norm1 opp_seg_id opp_x opp_ref_cy pt2_plan))
                 )
               )
               (setq sec_char_code (1+ sec_char_code))
             )
             (progn
               ;; 정상 범위 내 클릭(또는 Enter) 시: 평면도 광선 추적으로 맞은편 벽체 자동 탐색
               (setq plan_res
                 (vl-catch-all-apply 'drg-elev-x-to-plan-pt
                   (list ref_x seg_id seg-list seg_span_map ent_bound is_closed is_forward
                         base_start_dist total_bound_dist bnd-orient)))
               (if (vl-catch-all-error-p plan_res) (setq plan_res nil))
               (setq pt1_plan (if plan_res (nth 0 plan_res) nil))
               (setq in_norm1 (if plan_res (nth 2 plan_res) 0.0))

               (setq opp_hit
                 (if pt1_plan
                   (vl-catch-all-apply 'drg-find-opposite-section-hit
                     (list seg_id pt1_plan in_norm1 seg-list seg_span_map ent_bound is_closed is_forward
                           base_start_dist total_bound_dist bnd-orient))
                   nil
                 )
               )
               (if (vl-catch-all-error-p opp_hit) (setq opp_hit nil))
               (if (and opp_hit (or (= (nth 0 opp_hit) seg_id) (member (nth 0 opp_hit) sec_pair_done)))
                 (setq opp_hit nil)
               )

               (if opp_hit
                 (progn
                   (setq opp_seg_id (nth 0 opp_hit)
                         pt2_plan   (nth 1 opp_hit)
                         opp_x      (nth 2 opp_hit))
                   (setq opp_sp (assoc opp_seg_id seg_span_map))
                   (if opp_sp
                     (setq opp_x (max (float (nth 1 opp_sp)) (min (float (nth 2 opp_sp)) opp_x)))
                   )
                   (setq opp_ref_cy (get-y-on-pline ent_grnd opp_x))
                   (if (null opp_ref_cy) (setq opp_ref_cy ref_cy))

                   (if (not (assoc opp_seg_id seg_ref_cy_map))
                     (setq seg_ref_cy_map (append seg_ref_cy_map (list (cons opp_seg_id opp_ref_cy))))
                   )

                   (setq sec_pair_done (append sec_pair_done (list seg_id opp_seg_id)))
                   (setq section_pair_list
                     (append section_pair_list
                       (list (list letter_str seg_id ref_x ref_cy pt1_plan in_norm1 opp_seg_id opp_x opp_ref_cy pt2_plan))
                     )
                   )
                   (setq sec_char_code (1+ sec_char_code))
                   (princ (strcat "\n  -> [관통단면 " letter_str "-" letter_str " 자동매칭] Seg-" (itoa seg_id)
                                  " 위치를 버팀보 방향으로 투영하여 맞은편 Seg-" (itoa opp_seg_id)
                                  " 단면 위치(X=" (rtos opp_x 2 1) ", GL Y=" (rtos opp_ref_cy 2 2) ")를 자동 확정했습니다."))
                 )
                 (progn
                   (setq sec_pair_done (append sec_pair_done (list seg_id)))
                   (setq section_pair_list
                     (append section_pair_list
                       (list (list letter_str seg_id ref_x ref_cy pt1_plan in_norm1 nil nil nil nil))
                     )
                   )
                   (setq sec_char_code (1+ sec_char_code))
                 )
               )
             )
           )
         )
       )
      )
    )
  )

  ;; 4-1-B. SOIL-DATA가 들어있는 모든 세그먼트마다 기준단면 위치(ref_x, ref_cy)에 지반 주상도 블록 자동 생성
  (setq soil_col_ents nil)
  (setq proj_name
    (if (and (boundp '*tsp-current-project-name*) *tsp-current-project-name* (/= *tsp-current-project-name* ""))
      *tsp-current-project-name*
      "Project"
    )
  )
  (setq seg_idx 0)
  (while (< seg_idx (length seg-list))
    (setq seg_obj (nth seg_idx seg-list))
    (setq seg_id (1+ seg_idx))
    (if (and (assoc 'SOIL-DATA seg_obj) (cdr (assoc 'SOIL-DATA seg_obj)))
      (progn
        (setq ref_pair (assoc seg_id seg_ref_pt_map))
        (if ref_pair
          (progn
            (setq ref_x (nth 1 ref_pair)
                  ref_cy (nth 2 ref_pair))
            ;; _HRZ 기준선과 _GRND 지표면 Y좌표 차이로 실제 표고(EL) 산출
            (setq top_el_val (/ (- ref_cy hrz_y) 1000.0))
            (setq sc_ent
              (vl-catch-all-apply 'drg-create-and-insert-soil-column-block
                (list ref_x ref_cy top_el_val seg_obj proj_name)))
            (if (vl-catch-all-error-p sc_ent)
              (progn
                (princ (strcat "\n[단면진단] 주상도 생성 실패 Seg-" (itoa seg_id) " : "
                               (vl-catch-all-error-message sc_ent)
                               " / ref_x=" (rtos ref_x 2 1)
                               " ref_cy=" (rtos ref_cy 2 2)
                               " EL=" (vl-princ-to-string top_el_val)
                               " 지층수=" (vl-princ-to-string (length (cdr (assoc 'SOIL-DATA seg_obj))))
                               " HOLE=" (vl-princ-to-string (cdr (assoc 'HOLE-NO seg_obj)))))
                (setq sc_ent nil)
              )
            )
            (if sc_ent
              (setq soil_col_ents (append soil_col_ents (list sc_ent)))
            )
          )
        )
      )
    )
    (setq seg_idx (1+ seg_idx))
  )

  ;; 4-2. 스팬별 토류판(_토류판(timber)), CIP(_CIP), 숏크리트(_숏크리트(shotcrete)), 그라우팅(_그라우팅) 해치 작도
  ;;   - TSP 단면도(TSP_3B_Section.lsp / TSP_3C_Section.lsp)와 레이어, 색상, 해치 패턴 및 축척 100% 통일
  ;;   - 실제 지반선(_GRND) 경사를 평행하게 추종하여 암반선/근입깊이/숏크리트/그라우팅 깊이 산정
  ;;   - 겹치는 그라우팅 구간은 외곽 폴리선 중복 없이 하나의 해치로 병합하고, 이격된 그라우팅(예: CIP 사이 D200 자동배열)은 폭 D만큼 개별 작도
  (create_layer "_토류판(timber)" 1)
  (create_layer "_CIP" 7)
  (create_layer "_숏크리트(shotcrete)" 7)
  (create_layer "_그라우팅" 5)
  (setq hatch_ents nil)
  (setq grout_ents nil)
  (setq h_scl_fac (drg-get-hatch-scale-factor))

  (foreach sp tsp_spans
    (setq sec_start_x (nth 0 sp) sec_end_x (nth 1 sp) cur_depth (nth 2 sp))
    (setq seg_id (nth 5 sp) w_type (nth 6 sp) max_d (nth 7 sp) soil_lst (nth 10 sp) rock_emb (nth 11 sp))
    (setq w_chk (nth 16 sp) w_dep (nth 17 sp) sc_en (nth 18 sp) sc_chk (nth 19 sp) sc_lyr (nth 20 sp))
    (setq gr_idx (nth 21 sp) gr_sp_idx (nth 22 sp) gr_sp_cus (nth 23 sp))
    (setq cip_dia_str (nth 24 sp) cip_mode_idx (nth 25 sp) cip_ovl_str (nth 26 sp))

    ;; 숏크리트 및 그라우팅 기준 암반 시작 심도(m) 탐색 (TSP_1C 기준)
    (setq rock_d (drg-get-rock-depth-from-soil soil_lst sc_en sc_chk sc_lyr))
    (setq user_emb_mm (if (and rock_emb (/= rock_emb "")) (atof (vl-princ-to-string rock_emb)) 500.0))

    ;; [A] 벽체(CIP / 토류판) 및 숏크리트(_숏크리트(shotcrete)) 해치 작도 (실제 _GRND 지반선 경사 평행 추종)
    (if (= w_type "CIP")
      (progn
        (if (and rock_d (< rock_d max_d))
          (progn
            (setq cip_rel_depth_mm (+ (* rock_d 1000.0) user_emb_mm))
            (setq pts_cip (drg-sample-band-polygon ent_grnd ent_excav sec_start_x sec_end_x "GRND_TO_REL_DEPTH" nil cip_rel_depth_mm 0.0))
            (if pts_cip
              (setq hatch_ents (append hatch_ents (drg-make-closed-poly-and-hatch pts_cip "_CIP" 7 "AR-CONC" (* 6.0 h_scl_fac) 0.0)))
            )
            (setq pts_sc (drg-sample-band-polygon ent_grnd ent_excav sec_start_x sec_end_x "REL_DEPTH_TO_EXC" cip_rel_depth_mm nil 0.0))
            (if pts_sc
              (setq hatch_ents (append hatch_ents (drg-make-closed-poly-and-hatch pts_sc "_숏크리트(shotcrete)" 7 "AR-CONC" (* 2.0 h_scl_fac) 0.0)))
            )
          )
          (progn
            (setq pts_cip (drg-sample-band-polygon ent_grnd ent_excav sec_start_x sec_end_x "GRND_TO_PILE" nil nil cur_depth))
            (if pts_cip
              (setq hatch_ents (append hatch_ents (drg-make-closed-poly-and-hatch pts_cip "_CIP" 7 "AR-CONC" (* 6.0 h_scl_fac) 0.0)))
            )
          )
        )
      )
      (progn
        (if (and rock_d (< rock_d max_d))
          (progn
            (setq timber_rel_depth_mm (* rock_d 1000.0))
            (setq pts_timber (drg-sample-band-polygon ent_grnd ent_excav sec_start_x sec_end_x "GRND_TO_REL_DEPTH" nil timber_rel_depth_mm 0.0))
            (if pts_timber
              (setq hatch_ents (append hatch_ents (drg-make-closed-poly-and-hatch pts_timber "_토류판(timber)" 1 "ANSI31" (* 100.0 h_scl_fac) (* 135.0 (/ pi 180.0)))))
            )
            (setq pts_sc (drg-sample-band-polygon ent_grnd ent_excav sec_start_x sec_end_x "REL_DEPTH_TO_EXC" timber_rel_depth_mm nil 0.0))
            (if pts_sc
              (setq hatch_ents (append hatch_ents (drg-make-closed-poly-and-hatch pts_sc "_숏크리트(shotcrete)" 7 "AR-CONC" (* 2.0 h_scl_fac) 0.0)))
            )
          )
          (progn
            (setq pts_timber (drg-sample-band-polygon ent_grnd ent_excav sec_start_x sec_end_x "GRND_TO_EXC" nil nil 0.0))
            (if pts_timber
              (setq hatch_ents (append hatch_ents (drg-make-closed-poly-and-hatch pts_timber "_토류판(timber)" 1 "ANSI31" (* 100.0 h_scl_fac) (* 135.0 (/ pi 180.0)))))
            )
          )
        )
      )
    )

    ;; [B] 차수 그라우팅(_그라우팅) 해치 작도 (단면도 조건: 지하수위 활성 & 수위심도 < 굴착깊이 & 그라우팅 != "2")
    (setq water_lvl (if (and w_chk (= w_chk "1") w_dep) (float w_dep) 999.0))
    (if (and (/= (if gr_idx gr_idx "0") "2") (> max_d water_lvl))
      (progn
        (setq g_dia (if (= gr_idx "1") 600.0 200.0))
        (setq raw_g_ivs nil)

        ;; 1순위: 평면도 투영 모드(P)에서 추출된 실제 평면 그라우팅 객체 중심 투영값 사용
        (if (and (= place_opt "P") raw_grouts_info)
          (foreach g_info raw_grouts_info
            (setq gx (car g_info) gd (cadr g_info))
            (if (and (>= gx (- sec_start_x gd)) (<= gx (+ sec_end_x gd)))
              (setq raw_g_ivs (append raw_g_ivs (list (list (- gx (/ gd 2.0)) (+ gx (/ gd 2.0))))))
            )
          )
        )

        ;; 2순위: C 모드(세그먼트 자동배치)이거나 평면도에 그라우팅 객체가 없을 때 세그먼트 제원으로 자동 배열
        (if (null raw_g_ivs)
          (progn
            (setq c_dia_val (if (and cip_dia_str (/= cip_dia_str "")) (atof (vl-princ-to-string cip_dia_str)) 500.0))
            (setq c_ovl_val (if (and cip_ovl_str (/= cip_ovl_str "")) (atof (vl-princ-to-string cip_ovl_str)) 0.0))
            (setq c_ctc_val (if (= cip_mode_idx "0") c_dia_val (- c_dia_val c_ovl_val)))
            (if (<= c_ctc_val 50.0) (setq c_ctc_val 500.0))

            (if (and (= w_type "CIP") (= g_dia 200.0) (= gr_sp_idx "4"))
              ;; CIP + D200 자동(접점) 모드: CIP 두 공 사이 중점마다 D200 배치
              (progn
                (setq gx (+ sec_start_x (/ c_ctc_val 2.0)))
                (while (<= gx (- sec_end_x 10.0))
                  (setq raw_g_ivs (append raw_g_ivs (list (list (- gx (/ g_dia 2.0)) (+ gx (/ g_dia 2.0))))))
                  (setq gx (+ gx c_ctc_val))
                )
              )
              ;; 일반 간격 모드 (0:200, 1:400, 2:600, 3:직접입력)
              (progn
                (setq g_space
                  (cond
                    ((= gr_sp_idx "0") 200.0)
                    ((= gr_sp_idx "1") 400.0)
                    ((= gr_sp_idx "2") 600.0)
                    (t (if (and gr_sp_cus (/= gr_sp_cus "")) (atof (vl-princ-to-string gr_sp_cus)) 200.0))
                  )
                )
                (if (<= g_space 10.0) (setq g_space 200.0))
                (setq gx sec_start_x)
                (while (<= gx (+ sec_end_x 1.0))
                  (setq raw_g_ivs (append raw_g_ivs (list (list (- gx (/ g_dia 2.0)) (+ gx (/ g_dia 2.0))))))
                  (setq gx (+ gx g_space))
                )
              )
            )
          )
        )

        ;; 서로 겹치거나 맞닿는 그라우팅 구간은 단일 구간으로 병합, 떨어진 구간은 개별 폭(D) 유지
        (setq merged_g_ivs (drg-merge-grout-intervals raw_g_ivs sec_start_x sec_end_x))

        ;; 그라우팅 심도 산정 (단면도 TSP_3B / TSP_3C 와 100% 동일: 암반선이 굴착심도 위면 암반선+근입깊이, 아니면 말뚝하단)
        (foreach giv merged_g_ivs
          (setq gx1 (car giv) gx2 (cadr giv))
          (if (and rock_d (< rock_d max_d))
            (progn
              (setq grout_rel_depth_mm (+ (* rock_d 1000.0) user_emb_mm))
              (setq pts_grout (drg-sample-band-polygon ent_grnd ent_excav gx1 gx2 "GRND_TO_REL_DEPTH" nil grout_rel_depth_mm 0.0))
            )
            (setq pts_grout (drg-sample-band-polygon ent_grnd ent_excav gx1 gx2 "GRND_TO_PILE" nil nil cur_depth))
          )
          (if pts_grout
            (setq grout_ents (append grout_ents (drg-make-closed-poly-and-hatch pts_grout "_그라우팅" 5 "AR-SAND" (* 6.0 h_scl_fac) 0.0)))
          )
        )
      )
    )
  )

  ;; 4-3. 측면말뚝 일괄 작도
  (setq drawn_piles_info nil)
  (create_layer "_측면말뚝" 3)

  (foreach sp tsp_spans
    (setq sec_start_x (nth 0 sp))
    (setq sec_end_x (nth 1 sp))
    (setq cur_depth (nth 2 sp))
    (setq def_pw (nth 3 sp))
    (setq ctc (nth 4 sp))

    (setq cur_piles_info nil)
    (if (and (= place_opt "P") raw_piles_info)
      (foreach p_info raw_piles_info
        (setq px (car p_info) pw (cadr p_info))
        (if (and (>= px (- sec_start_x 1.0)) (<= px (+ sec_end_x 1.0)))
          (setq cur_piles_info (append cur_piles_info (list (list px (if pw pw def_pw)))))
        )
      )
      (progn
        (setq last_pile_x sec_start_x)
        (setq cur_piles_info (append cur_piles_info (list (list sec_start_x def_pw))))
        (setq curr_x (+ last_pile_x ctc))
        (while (< curr_x (- sec_end_x 1.0))
          (setq cur_piles_info (append cur_piles_info (list (list curr_x def_pw))))
          (setq last_pile_x curr_x)
          (setq curr_x (+ last_pile_x ctc))
        )
        (setq cur_piles_info (append cur_piles_info (list (list sec_end_x def_pw))))
      )
    )

    (foreach p_info cur_piles_info
      (setq px (car p_info))
      (setq ext_pw (cadr p_info))
      (setq cur_pw (if ext_pw ext_pw def_pw))
      (setq top_y (get-y-on-pline ent_grnd px))
      (setq bot_y (get-y-on-pline ent_excav px))
      (if bot_y (setq bot_y (- bot_y cur_depth)))

      (if (and top_y bot_y)
        (progn
          (setq existing_pile nil e_idx 0 found_idx -1)
          (while (< e_idx (length drawn_piles_info))
            (setq ep (nth e_idx drawn_piles_info))
            (if (< (abs (- (car ep) px)) 1.0)
              (progn (setq existing_pile ep) (setq found_idx e_idx))
            )
            (setq e_idx (1+ e_idx))
          )

          (if existing_pile
            (progn
              (setq old_top_y (nth 1 existing_pile))
              (setq old_bot_y (nth 2 existing_pile))
              (setq old_pw (nth 3 existing_pile))
              (setq old_poly (nth 4 existing_pile))
              (setq old_line (nth 5 existing_pile))
              (setq old_ghost (nth 6 existing_pile))

              (setq final_bot_y (min bot_y old_bot_y))
              (setq final_pw (max cur_pw old_pw))

              (if (or (< final_bot_y (- old_bot_y 1.0)) (> final_pw (+ old_pw 1.0)))
                (progn
                  (if (and old_poly (entget old_poly)) (entdel old_poly))
                  (if (and old_line (entget old_line)) (entdel old_line))
                  (setq half_w (/ final_pw 2.0))
                  (setq pt1 (list (- px half_w) final_bot_y) pt2 (list (+ px half_w) final_bot_y)
                        pt3 (list (+ px half_w) old_top_y) pt4 (list (- px half_w) old_top_y))
                  (setq new_poly (entmakex (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1) '(8 . "_측면말뚝") (cons 10 pt1) (cons 10 pt2) (cons 10 pt3) (cons 10 pt4))))
                  (setq new_line (entmakex (list '(0 . "LINE") '(8 . "_측면말뚝") (cons 10 (list px final_bot_y)) (cons 11 (list px old_top_y)))))
                  (setq new_ep (list px old_top_y final_bot_y final_pw new_poly new_line old_ghost))
                  (setq temp_list nil e_idx 0)
                  (while (< e_idx (length drawn_piles_info))
                    (if (= e_idx found_idx)
                      (setq temp_list (append temp_list (list new_ep)))
                      (setq temp_list (append temp_list (list (nth e_idx drawn_piles_info))))
                    )
                    (setq e_idx (1+ e_idx))
                  )
                  (setq drawn_piles_info temp_list)
                )
              )
            )
            (progn
              (setq half_w (/ cur_pw 2.0))
              (setq pt1 (list (- px half_w) bot_y) pt2 (list (+ px half_w) bot_y)
                    pt3 (list (+ px half_w) top_y) pt4 (list (- px half_w) top_y))
              (setq new_poly (entmakex (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1) '(8 . "_측면말뚝") (cons 10 pt1) (cons 10 pt2) (cons 10 pt3) (cons 10 pt4))))
              (setq new_line (entmakex (list '(0 . "LINE") '(8 . "_측면말뚝") (cons 10 (list px bot_y)) (cons 11 (list px top_y)))))
              (setq drawn_piles_info (append drawn_piles_info (list (list px top_y bot_y cur_pw new_poly new_line nil))))
            )
          )
        )
      )
    )
  )

  ;; 4-4. 주구간 띠장(Wale) 직사각형 및 일반 지보재 블록(버팀보/앵커) 작도 + 맞은편 세그먼트(방식 B) 자동 연동
  (create_layer "_띠장(Wale)" 3)
  (setq block_ents nil)
  (setq drawn_wale_tracker nil)

  (foreach sp tsp_spans
    (setq sec_start_x (nth 0 sp) sec_end_x (nth 1 sp) ctc (nth 4 sp))
    (setq seg_id (nth 5 sp) wale_sp (nth 8 sp) sup_lst (nth 9 sp))
    (setq seg_c1_x (nth 13 sp) seg_c2_x (nth 14 sp) scale_f (nth 15 sp))
    (setq ref_cy (cdr (assoc seg_id seg_ref_cy_map)))
    (setq seg_obj (nth (1- seg_id) seg-list))
    (setq seg_v_s (cdr (assoc 'V-START seg_obj)) seg_v_a (cdr (assoc 'ANGLE seg_obj)) seg_len_mm (cdr (assoc 'LENGTH seg_obj)))

    (foreach s_item sup_lst
      (if (and (listp s_item) (> (length s_item) 3))
        (progn
          (setq s_type (car s_item))
          (setq s_depth (float (cadddr s_item)))
          (setq ins_y (- ref_cy (* s_depth 1000.0)))

          (setq sup_x1 sec_start_x sup_x2 sec_end_x)
          (setq r_pts (if (wcmatch s_type "앵커*")
                        (if (> (length s_item) 11) (nth 11 s_item) nil)
                        (if (> (length s_item) 13) (nth 13 s_item) nil)))
          (if (and r_pts (listp r_pts) (= (length r_pts) 2) seg_v_s seg_v_a)
            (progn
              (setq dx (- (car (car r_pts)) (car seg_v_s)) dy (- (cadr (car r_pts)) (cadr seg_v_s)))
              (setq dp1 (+ (* dx (cos seg_v_a)) (* dy (sin seg_v_a))))
              (setq dx (- (car (cadr r_pts)) (car seg_v_s)) dy (- (cadr (cadr r_pts)) (cadr seg_v_s)))
              (setq dp2 (+ (* dx (cos seg_v_a)) (* dy (sin seg_v_a))))
              (setq iv_d1 (max 0.0 (min seg_len_mm (min dp1 dp2))))
              (setq iv_d2 (max 0.0 (min seg_len_mm (max dp1 dp2))))
              (setq sup_x1 (max sec_start_x (+ seg_c1_x (* iv_d1 scale_f))))
              (setq sup_x2 (min sec_end_x (+ seg_c1_x (* iv_d2 scale_f))))
            )
          )

          (if (> (- sup_x2 sup_x1) 10.0)
            (progn
              (setq item_wale_sp
                (if (wcmatch s_type "앵커*")
                  (if (and (> (length s_item) 10) (nth 10 s_item)) (nth 10 s_item) wale_sp)
                  (if (and (> (length s_item) 12) (nth 12 s_item)) (nth 12 s_item) wale_sp)
                )
              )
              (setq wale_b (drg-parse-wale-height item_wale_sp))
              (setq is_2h_wale (and item_wale_sp (wcmatch (strcase item_wale_sp) "2H *")))

              ;; [주구간 띠장 직사각형 작도]
              (cond
                ((wcmatch s_type "앵커*")
                 (drg-draw-wale-rect sup_x1 sup_x2 (+ ins_y 25.0) (+ ins_y 25.0 wale_b) "_띠장(Wale)" 3)
                 (drg-draw-wale-rect sup_x1 sup_x2 (- ins_y 25.0 wale_b) (- ins_y 25.0) "_띠장(Wale)" 3)
                )
                (is_2h_wale
                 (drg-draw-wale-rect sup_x1 sup_x2 ins_y (+ ins_y wale_b) "_띠장(Wale)" 3)
                 (drg-draw-wale-rect sup_x1 sup_x2 (- ins_y wale_b) ins_y "_띠장(Wale)" 3)
                )
                (t
                 (drg-draw-wale-rect sup_x1 sup_x2 (- ins_y (/ wale_b 2.0)) (+ ins_y (/ wale_b 2.0)) "_띠장(Wale)" 3)
                )
              )
              (setq drawn_wale_tracker (append drawn_wale_tracker (list (list seg_id ins_y))))

              ;; [일반 지보재(앵커/버팀보) 블록 삽입]
              (cond
                ((wcmatch s_type "앵커*")
                 (if (and (= place_opt "P") proj_anchor_xs)
                   (foreach ax proj_anchor_xs
                     (if (and (>= ax (- sup_x1 50.0)) (<= ax (+ sup_x2 50.0)))
                       (progn
                         (setq b_ent (drg-insert-support-block-centered "앵커(Anchor)" "_앵커(Anchor)" ax ins_y))
                         (if b_ent (setq block_ents (append block_ents (list b_ent))))
                       )
                     )
                   )
                   (progn
                     (setq cur_sx (+ sup_x1 (/ ctc 2.0)))
                     (while (<= cur_sx (- sup_x2 100.0))
                       (setq b_ent (drg-insert-support-block-centered "앵커(Anchor)" "_앵커(Anchor)" cur_sx ins_y))
                       (if b_ent (setq block_ents (append block_ents (list b_ent))))
                       (setq cur_sx (+ cur_sx ctc))
                     )
                   )
                 )
                )
                ((wcmatch s_type "버팀보*")
                 (setq s_cnt (nth 4 s_item))
                 (setq s_spec (strcase (nth 6 s_item)))
                 (setq is_2h_strut (or (and (numberp s_cnt) (>= s_cnt 2)) (wcmatch s_spec "*2H*")))
                 (setq blk_to_use (if is_2h_strut "버팀보(Strut)_2H300x300" "버팀보(Strut)_H300x300"))

                 (if (and (= place_opt "P") proj_strut_xs)
                   (progn
                     (foreach sx proj_strut_xs
                       (if (and (>= sx (- sup_x1 50.0)) (<= sx (+ sup_x2 50.0)))
                         (progn
                           (setq b_ent (drg-insert-support-block-centered blk_to_use "_버팀보(Strut)" sx ins_y))
                           (if b_ent (setq block_ents (append block_ents (list b_ent))))
                         )
                       )
                     )
                   )
                   (progn
                     (setq s_ctc_mm (* (if (and (> (length s_item) 10) (nth 10 s_item)) (atof (vl-princ-to-string (nth 10 s_item))) 5.0) 1000.0))
                     (if (<= s_ctc_mm 100.0) (setq s_ctc_mm 5000.0))
                     (setq cur_sx (+ sup_x1 (/ s_ctc_mm 2.0)))
                     (while (<= cur_sx (- sup_x2 200.0))
                       (setq b_ent (drg-insert-support-block-centered blk_to_use "_버팀보(Strut)" cur_sx ins_y))
                       (if b_ent (setq block_ents (append block_ents (list b_ent))))
                       (setq cur_sx (+ cur_sx s_ctc_mm))
                     )
                   )
                 )

                 ;; [맞은편 세그먼트(예: Seg-1 -> Seg-3) 버팀보 및 띠장 자동 역추적 연동 (방식 B)]
                 (setq opp_hits (drg-find-opposite-strut-targets (1- seg_id) s_item seg-list seg_span_map bnd-orient))
                 (foreach opp_entry opp_hits
                   (setq opp_seg_id (car opp_entry)
                         opp_ray_xs (cadr opp_entry))
                   (setq opp_sp (assoc opp_seg_id seg_span_map))
                   (setq opp_seg_obj (nth (1- opp_seg_id) seg-list))
                   (if (and opp_sp opp_seg_obj)
                     (progn
                       (setq opp_c1_x (nth 1 opp_sp)
                             opp_c2_x (nth 2 opp_sp))
                       (setq opp_sup_lst (cdr (assoc 'SUPPORT-LIST opp_seg_obj)))

                       (setq has_own_strut_at_depth
                         (vl-some
                           '(lambda (os)
                              (and (listp os) (> (length os) 3)
                                   (wcmatch (car os) "버팀보*")
                                   (< (abs (- (float (cadddr os)) s_depth)) 0.1))
                            )
                           opp_sup_lst
                         )
                       )

                       (if (not has_own_strut_at_depth)
                         (progn
                           (setq wale_already_drawn
                             (vl-some
                               '(lambda (tr) (and (= (car tr) opp_seg_id) (< (abs (- (cadr tr) ins_y)) 50.0)))
                               drawn_wale_tracker
                             )
                           )
                           (if (not wale_already_drawn)
                             (progn
                               (setq opp_wale_sp (if item_wale_sp item_wale_sp (cdr (assoc 'WALE-SPEC opp_seg_obj))))
                               (setq opp_wale_b (drg-parse-wale-height opp_wale_sp))
                               (setq opp_is_2h_wale (and opp_wale_sp (wcmatch (strcase opp_wale_sp) "2H *")))
                               (if opp_is_2h_wale
                                 (progn
                                   (drg-draw-wale-rect opp_c1_x opp_c2_x ins_y (+ ins_y opp_wale_b) "_띠장(Wale)" 3)
                                   (drg-draw-wale-rect opp_c1_x opp_c2_x (- ins_y opp_wale_b) ins_y "_띠장(Wale)" 3)
                                 )
                                 (drg-draw-wale-rect opp_c1_x opp_c2_x (- ins_y (/ opp_wale_b 2.0)) (+ ins_y (/ opp_wale_b 2.0)) "_띠장(Wale)" 3)
                               )
                               (setq drawn_wale_tracker (append drawn_wale_tracker (list (list opp_seg_id ins_y))))
                             )
                           )

                           (setq p_xs_in_opp nil)
                           (if (and (= place_opt "P") proj_strut_xs)
                             (foreach sx proj_strut_xs
                               (if (and (>= sx (- opp_c1_x 50.0)) (<= sx (+ opp_c2_x 50.0)))
                                 (setq p_xs_in_opp (append p_xs_in_opp (list sx)))
                               )
                             )
                           )
                           (setq xs_to_draw (if p_xs_in_opp p_xs_in_opp opp_ray_xs))
                           (foreach ox xs_to_draw
                             (if (and (>= ox (- opp_c1_x 50.0)) (<= ox (+ opp_c2_x 50.0)))
                               (progn
                                 (setq b_ent (drg-insert-support-block-centered blk_to_use "_버팀보(Strut)" ox ins_y))
                                 (if b_ent (setq block_ents (append block_ents (list b_ent))))
                               )
                             )
                           )
                         )
                       )
                     )
                   )
                 )
                )
              )
            )
          )
        )
      )
    )
  )

  ;; 4-5. [방향 B + 방식 1] 모서리 사보강재(Corner Strut) 단별 설치깊이 기준 독립 작도
  (setq seg_idx 0 num_segs (length seg-list))
  (while (< seg_idx num_segs)
    (setq cur_seg (nth seg_idx seg-list))
    (setq c_brace (cdr (assoc 'CORNER-BRACE cur_seg)))
    (setq c_stages (drg-get-corner-brace-stages c_brace cur_seg))

    (if (> (length c_stages) 0)
      (progn
        (setq next_seg_id (1+ seg_idx))
        (setq prev_idx (if (= seg_idx 0) (if is_closed (1- num_segs) -1) (1- seg_idx)))
        (setq prev_seg_id (if (>= prev_idx 0) (1+ prev_idx) -1))

        (setq next_sp (assoc next_seg_id seg_span_map))
        (setq prev_sp (if (> prev_seg_id 0) (assoc prev_seg_id seg_span_map) nil))

        (setq c_upg (cdr (assoc 'UPGRADE-WALE cur_seg)))
        (setq upg_spec (if (and c_upg (= (car c_upg) "Y")) (cadr c_upg) nil))

        (setq corner_x
          (cond
            (next_sp (nth 1 next_sp))
            (prev_sp (nth 2 prev_sp))
            ((and corner_x_list (> (length corner_x_list) seg_idx)) (nth seg_idx corner_x_list))
            (t prof_start_x)
          )
        )
        (setq corner_grnd_y (get-y-on-pline ent_grnd corner_x))
        (if (null corner_grnd_y)
          (setq corner_grnd_y (cdr (assoc next_seg_id seg_ref_cy_map)))
        )
        (if (null corner_grnd_y)
          (setq corner_grnd_y (cadr (vlax-curve-getStartPoint ent_grnd)))
        )

        (foreach st c_stages
          (setq st_depth (float (nth 1 st)))
          (setq st_rows (nth 2 st))
          (setq ins_y (- corner_grnd_y (* st_depth 1000.0)))

          (if (and st_rows (> (length st_rows) 0))
            (progn
              (setq max_b_dist 0.0 max_row_2h nil)
              (foreach r st_rows
                (if (> (float (nth 2 r)) max_b_dist) (setq max_b_dist (float (nth 2 r))))
                (if (wcmatch (strcase (nth 0 r)) "*2H*") (setq max_row_2h T))
              )

              ;; [1] 모서리 좌측(이전 세그먼트 끝단: seg_c2_x 기준 좌측으로 b_dist 이격) 작도
              (if (and prev_sp (cdr (assoc 'IS-DEFINED (nth prev_idx seg-list))))
                (progn
                  (setq seg_c1_x (nth 1 prev_sp) seg_c2_x (nth 2 prev_sp) scale_f (nth 3 prev_sp))
                  (setq prev_ref_cy (cdr (assoc prev_seg_id seg_ref_cy_map)))
                  (if (null prev_ref_cy) (setq prev_ref_cy corner_grnd_y))
                  (setq sup_lst (cdr (assoc 'SUPPORT-LIST (nth prev_idx seg-list))))

                  (setq has_same_depth_prev
                    (or
                      (vl-some
                        '(lambda (s)
                           (and (listp s) (> (length s) 3)
                                (< (abs (- (- prev_ref_cy (* (float (cadddr s)) 1000.0)) ins_y)) 50.0))
                         )
                        sup_lst
                      )
                      (vl-some
                        '(lambda (tr) (and (= (car tr) prev_seg_id) (< (abs (- (cadr tr) ins_y)) 50.0)))
                        drawn_wale_tracker
                      )
                    )
                  )

                  (if (not has_same_depth_prev)
                    (progn
                      (setq cw_b (drg-parse-wale-height (if upg_spec upg_spec (cdr (assoc 'WALE-SPEC (nth prev_idx seg-list))))))
                      (setq cw_x1 (max seg_c1_x (- seg_c2_x (* (+ (* max_b_dist 1000.0) 500.0) scale_f))))
                      (setq cw_x2 seg_c2_x)
                      (if max_row_2h
                        (progn
                          (drg-draw-wale-rect cw_x1 cw_x2 ins_y (+ ins_y cw_b) "_띠장(Wale)" 3)
                          (drg-draw-wale-rect cw_x1 cw_x2 (- ins_y cw_b) ins_y "_띠장(Wale)" 3)
                        )
                        (drg-draw-wale-rect cw_x1 cw_x2 (- ins_y (/ cw_b 2.0)) (+ ins_y (/ cw_b 2.0)) "_띠장(Wale)" 3)
                      )
                    )
                  )

                  (foreach r st_rows
                    (setq b_dist_mm (* (float (nth 2 r)) 1000.0))
                    (setq cx_prev (- seg_c2_x (* b_dist_mm scale_f)))
                    (if (>= cx_prev (- seg_c1_x 10.0))
                      (progn
                        (setq row_is_2h (wcmatch (strcase (nth 0 r)) "*2H*"))
                        (setq blk_to_use (if row_is_2h "사보강재(ConerStrut)_2H300x300" "사보강재(ConerStrut)_H300x300"))
                        (setq b_ent (drg-insert-support-block-centered blk_to_use "_사보강재(ConerStrut)" cx_prev ins_y))
                        (if b_ent (setq block_ents (append block_ents (list b_ent))))
                      )
                    )
                  )
                )
              )

              ;; [2] 모서리 우측(현재 세그먼트 시작단: seg_c1_x 기준 우측으로 b_dist 이격) 작도
              (if (and next_sp (cdr (assoc 'IS-DEFINED cur_seg)))
                (progn
                  (setq seg_c1_x (nth 1 next_sp) seg_c2_x (nth 2 next_sp) scale_f (nth 3 next_sp))
                  (setq next_ref_cy (cdr (assoc next_seg_id seg_ref_cy_map)))
                  (if (null next_ref_cy) (setq next_ref_cy corner_grnd_y))
                  (setq sup_lst (cdr (assoc 'SUPPORT-LIST cur_seg)))

                  (setq has_same_depth_next
                    (or
                      (vl-some
                        '(lambda (s)
                           (and (listp s) (> (length s) 3)
                                (< (abs (- (- next_ref_cy (* (float (cadddr s)) 1000.0)) ins_y)) 50.0))
                         )
                        sup_lst
                      )
                      (vl-some
                        '(lambda (tr) (and (= (car tr) next_seg_id) (< (abs (- (cadr tr) ins_y)) 50.0)))
                        drawn_wale_tracker
                      )
                    )
                  )

                  (if (not has_same_depth_next)
                    (progn
                      (setq cw_b (drg-parse-wale-height (if upg_spec upg_spec (cdr (assoc 'WALE-SPEC cur_seg)))))
                      (setq cw_x1 seg_c1_x)
                      (setq cw_x2 (min seg_c2_x (+ seg_c1_x (* (+ (* max_b_dist 1000.0) 500.0) scale_f))))
                      (if max_row_2h
                        (progn
                          (drg-draw-wale-rect cw_x1 cw_x2 ins_y (+ ins_y cw_b) "_띠장(Wale)" 3)
                          (drg-draw-wale-rect cw_x1 cw_x2 (- ins_y cw_b) ins_y "_띠장(Wale)" 3)
                        )
                        (drg-draw-wale-rect cw_x1 cw_x2 (- ins_y (/ cw_b 2.0)) (+ ins_y (/ cw_b 2.0)) "_띠장(Wale)" 3)
                      )
                    )
                  )

                  (foreach r st_rows
                    (setq b_dist_mm (* (float (nth 2 r)) 1000.0))
                    (setq cx_next (+ seg_c1_x (* b_dist_mm scale_f)))
                    (if (<= cx_next (+ seg_c2_x 10.0))
                      (progn
                        (setq row_is_2h (wcmatch (strcase (nth 0 r)) "*2H*"))
                        (setq blk_to_use (if row_is_2h "사보강재(ConerStrut)_2H300x300" "사보강재(ConerStrut)_H300x300"))
                        (setq b_ent (drg-insert-support-block-centered blk_to_use "_사보강재(ConerStrut)" cx_next ins_y))
                        (if b_ent (setq block_ents (append block_ents (list b_ent))))
                      )
                    )
                  )
                )
              )

              ;; [3] 개방형 선형 등에서 인접 세그먼트가 미정의일 때의 보조 작도
              (if (and prev_sp (not (cdr (assoc 'IS-DEFINED (nth prev_idx seg-list)))))
                (progn
                  (setq seg_c1_x (nth 1 prev_sp) seg_c2_x (nth 2 prev_sp) scale_f (nth 3 prev_sp))
                  (setq cw_b (drg-parse-wale-height (if upg_spec upg_spec (cdr (assoc 'WALE-SPEC cur_seg)))))
                  (setq cw_x1 (max seg_c1_x (- seg_c2_x (* (+ (* max_b_dist 1000.0) 500.0) scale_f))))
                  (setq cw_x2 seg_c2_x)
                  (if max_row_2h
                    (progn
                      (drg-draw-wale-rect cw_x1 cw_x2 ins_y (+ ins_y cw_b) "_띠장(Wale)" 3)
                      (drg-draw-wale-rect cw_x1 cw_x2 (- ins_y cw_b) ins_y "_띠장(Wale)" 3)
                    )
                    (drg-draw-wale-rect cw_x1 cw_x2 (- ins_y (/ cw_b 2.0)) (+ ins_y (/ cw_b 2.0)) "_띠장(Wale)" 3)
                  )
                  (foreach r st_rows
                    (setq b_dist_mm (* (float (nth 2 r)) 1000.0))
                    (setq cx_prev (- seg_c2_x (* b_dist_mm scale_f)))
                    (if (>= cx_prev (- seg_c1_x 10.0))
                      (progn
                        (setq row_is_2h (wcmatch (strcase (nth 0 r)) "*2H*"))
                        (setq blk_to_use (if row_is_2h "사보강재(ConerStrut)_2H300x300" "사보강재(ConerStrut)_H300x300"))
                        (setq b_ent (drg-insert-support-block-centered blk_to_use "_사보강재(ConerStrut)" cx_prev ins_y))
                        (if b_ent (setq block_ents (append block_ents (list b_ent))))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    (setq seg_idx (1+ seg_idx))
  )

  ;; 5. 폐합선 마지막 허수(Ghost) 말뚝 처리
  (if is_closed
    (progn
      (setq end_circle_x final_target_x)
      (setq ghost_exists nil)
      (foreach ep drawn_piles_info
        (if (< (abs (- (car ep) end_circle_x)) 1.0)
          (setq ghost_exists T)
        )
      )
      (if (not ghost_exists)
        (progn
          (setq first_p (car drawn_piles_info))
          (if first_p
            (progn
              (setq g_bot_y (nth 2 first_p) g_pw (nth 3 first_p) g_top_y (get-y-on-pline ent_grnd end_circle_x))
              (setq half_w (/ g_pw 2.0))
              (setq pt1 (list (- end_circle_x half_w) g_bot_y) pt2 (list (+ end_circle_x half_w) g_bot_y)
                    pt3 (list (+ end_circle_x half_w) g_top_y) pt4 (list (- end_circle_x half_w) g_top_y))
              (setq g_poly (entmakex (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1) '(8 . "_측면말뚝") '(62 . 8) (cons 10 pt1) (cons 10 pt2) (cons 10 pt3) (cons 10 pt4))))
              (setq g_line (entmakex (list '(0 . "LINE") '(8 . "_측면말뚝") '(62 . 8) (cons 10 (list end_circle_x g_bot_y)) (cons 11 (list end_circle_x g_top_y)))))
              (setq drawn_piles_info (append drawn_piles_info (list (list end_circle_x g_top_y g_bot_y g_pw g_poly g_line T))))
            )
          )
        )
        (progn
          (setq e_idx 0 temp_list nil)
          (while (< e_idx (length drawn_piles_info))
            (setq ep (nth e_idx drawn_piles_info))
            (if (< (abs (- (car ep) end_circle_x)) 1.0)
              (progn
                (setq ep_poly (nth 4 ep) ep_line (nth 5 ep))
                (if ep_poly (entmod (append (entget ep_poly) (list '(62 . 8)))))
                (if ep_line (entmod (append (entget ep_line) (list '(62 . 8)))))
                (setq temp_list (append temp_list (list (list (car ep) (nth 1 ep) (nth 2 ep) (nth 3 ep) ep_poly ep_line T))))
              )
              (setq temp_list (append temp_list (list ep)))
            )
            (setq e_idx (1+ e_idx))
          )
          (setq drawn_piles_info temp_list)
        )
      )
    )
  )

  (setq final_piles nil)
  (foreach ep drawn_piles_info
    (if (not (nth 6 ep))
      (setq final_piles (append final_piles (list ep)))
    )
  )
  (setq final_piles (vl-sort final_piles '(lambda (a b) (< (car a) (car b)))))

  ;; 5-1. 전개도 상단 TSP 모서리 번호 + 캡슐(H-Pile 순번)은 4-0-A 단계에서 선행 작도 완료됨
  (setq *drg-top-ind-pre-drawn* nil)

  ;; 6. 10번째 측면말뚝마다 하단 캡슐 번호 생성
  (create_layer "_지시" 1)
  (defun draw-bottom-capsule (center-pt h-half w-half rad l-name / p-c1 p-c2 p-c3 p-c4 p-c5 p-c6 p-c7 p-c8)
    (setq p-c1 (list (+ (car center-pt) (- w-half rad)) (+ (cadr center-pt) h-half)))
    (setq p-c2 (list (+ (car center-pt) w-half) (+ (cadr center-pt) (- h-half rad))))
    (setq p-c3 (list (+ (car center-pt) w-half) (- (cadr center-pt) (- h-half rad))))
    (setq p-c4 (list (+ (car center-pt) (- w-half rad)) (- (cadr center-pt) h-half)))
    (setq p-c5 (list (- (car center-pt) (- w-half rad)) (- (cadr center-pt) h-half)))
    (setq p-c6 (list (- (car center-pt) w-half) (- (cadr center-pt) (- h-half rad))))
    (setq p-c7 (list (- (car center-pt) w-half) (+ (cadr center-pt) (- h-half rad))))
    (setq p-c8 (list (- (car center-pt) (- w-half rad)) (+ (cadr center-pt) h-half)))
    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                   (cons 8 l-name) '(62 . 1) '(90 . 8) '(70 . 1)
                   (cons 10 p-c1) '(42 . -0.41421356) (cons 10 p-c2) '(42 . 0.0)
                   (cons 10 p-c3) '(42 . -0.41421356) (cons 10 p-c4) '(42 . 0.0)
                   (cons 10 p-c5) '(42 . -0.41421356) (cons 10 p-c6) '(42 . 0.0)
                   (cons 10 p-c7) '(42 . -0.41421356) (cons 10 p-c8) '(42 . 0.0)))
  )

  (setq pile_idx 1)
  (foreach ep final_piles
    (if (= (rem pile_idx 10) 0)
      (progn
        (setq p_x (car ep) p_bot_y (nth 2 ep))
        (setq gap 1000.0 l_len 1000.0)
        (setq l_start (list p_x (- p_bot_y gap) 0.0))
        (setq l_end (list p_x (- p_bot_y (+ gap l_len)) 0.0))
        (entmake (list '(0 . "LINE") '(8 . "_지시") '(62 . 1) (cons 10 l_start) (cons 11 l_end)))
        (setq cur_txt_hgt (if (vlax-ldata-get "DRG_SETTINGS" "TXT_HGT") (vlax-ldata-get "DRG_SETTINGS" "TXT_HGT") 600.0))
        (setq cur_txt_sty (if (vlax-ldata-get "DRG_SETTINGS" "TXT_STY") (vlax-ldata-get "DRG_SETTINGS" "TXT_STY") "Standard"))
        (setq cap_h_half (* cur_txt_hgt (/ 500.0 600.0)))
        (setq cap_rad (* cur_txt_hgt (/ 400.0 600.0)))
        (setq cap_w_half (* (+ (* cur_txt_hgt 2.0) (* (strlen (itoa pile_idx)) (* cur_txt_hgt 0.5))) 0.5))
        (setq cap_center (list p_x (- (cadr l_end) cap_h_half) 0.0))
        (draw-bottom-capsule cap_center cap_h_half cap_w_half cap_rad "_지시")
        (entmake (list '(0 . "TEXT") '(8 . "_지시") '(62 . 7)
                       (cons 10 cap_center) (cons 11 cap_center)
                       (cons 40 cur_txt_hgt) (cons 50 0.0) (cons 1 (itoa pile_idx))
                       (cons 7 cur_txt_sty) '(72 . 1) '(73 . 2)))
      )
    )
    (setq pile_idx (1+ pile_idx))
  )

  ;; 7. 그리기 순서(DRAWORDER) 정리:
  ;;   1) 벽체 해치(토류판/CIP/숏크리트)를 뒤로(_B)
  ;;   2) 차수 그라우팅 해치(grout_ents)를 벽체 해치보다 더 뒤로(_B)
  ;;   3) 지반 주상도 블록(soil_col_ents)을 그보다 더 맨 뒤로(_B) 보내어 벽체/그라우팅/말뚝보다 뒤에 위치하도록 정렬
  ;;   4) 지보재 블록(block_ents)은 맨 앞으로(_F)
  (if hatch_ents
    (progn
      (setq ss_back (ssadd))
      (foreach e hatch_ents (if (and e (entget e)) (ssadd e ss_back)))
      (if (> (sslength ss_back) 0)
        (vl-catch-all-apply 'vl-cmdf (list "_.DRAWORDER" ss_back "" "_B"))
      )
    )
  )
  (if grout_ents
    (progn
      (setq ss_grout_back (ssadd))
      (foreach e grout_ents (if (and e (entget e)) (ssadd e ss_grout_back)))
      (if (> (sslength ss_grout_back) 0)
        (vl-catch-all-apply 'vl-cmdf (list "_.DRAWORDER" ss_grout_back "" "_B"))
      )
    )
  )
  (if soil_col_ents
    (progn
      (setq ss_soil_back (ssadd))
      (foreach e soil_col_ents (if (and e (entget e)) (ssadd e ss_soil_back)))
      (if (> (sslength ss_soil_back) 0)
        (vl-catch-all-apply 'vl-cmdf (list "_.DRAWORDER" ss_soil_back "" "_B"))
      )
    )
  )
  (if block_ents
    (progn
      (setq ss_front (ssadd))
      (foreach e block_ents (if (and e (entget e)) (ssadd e ss_front)))
      (if (> (sslength ss_front) 0)
        (vl-catch-all-apply 'vl-cmdf (list "_.DRAWORDER" ss_front "" "_F"))
      )
    )
  )

    ;; 7-B. 전개도 및 평면도에 '_단면표시' 기호(A-A, B-B...) 일괄 작도
  (princ (strcat "\n[단면진단] 7-B 진입 - 단면 페어 " (itoa (length section_pair_list)) "개"))
  ;;   - section_pair_list 는 4-1-A 에서 생성된 단면 페어 목록
  (setq sec_mark_ents nil)
  (setq s_factor_sec (/ (if (and (boundp '*tsp-scale-section*) *tsp-scale-section* (> *tsp-scale-section* 0))
                          (float *tsp-scale-section*)
                          200.0)
                        200.0))
  (foreach s_pair section_pair_list
    (setq letter_str (nth 0 s_pair)
          seg_id     (nth 1 s_pair)
          ref_x      (nth 2 s_pair)
          ref_cy     (nth 3 s_pair)
          pt1_plan   (nth 4 s_pair)
          in_norm1   (nth 5 s_pair)
          opp_seg_id (nth 6 s_pair)
          opp_x      (nth 7 s_pair)
          opp_ref_cy (nth 8 s_pair)
          pt2_plan   (nth 9 s_pair))

    ;; [1] 주 세그먼트(예: Seg-1 또는 Seg-2) 전개도 단면표시: "A (Seg-1)"
    (setq cur_seg_data (nth (1- seg_id) seg-list))
    (setq cur_emb_m (drg-extract-embed-from-zone cur_seg_data cur_seg_data))
    (setq top_box_y (+ ref_cy (* 1800.0 s_factor_sec)))
    (setq bot_pile_y (get-y-on-pline ent_excav ref_x))
    (if bot_pile_y
      (setq bot_pile_y (- bot_pile_y (* cur_emb_m 1000.0)))
      (setq bot_pile_y (- ref_cy 10000.0))
    )
    (setq sec_mark_ents
      (append sec_mark_ents
        (vl-catch-all-apply 'drg-draw-elevation-section-mark
          (list ref_x top_box_y bot_pile_y (strcat letter_str " (Seg-" (itoa seg_id) ")")))
      )
    )
    (if (vl-catch-all-error-p (last sec_mark_ents))
      (princ (strcat "\n[단면진단] 전개도 단면기호 실패 Seg-" (itoa seg_id) " : "
                     (vl-catch-all-error-message (last sec_mark_ents))
                     " / ref_x=" (rtos ref_x 2 1)
                     " top_y=" (vl-princ-to-string top_box_y)
                     " bot_y=" (vl-princ-to-string bot_pile_y)))
    )

    ;; [2] 맞은편 관통 세그먼트(예: Seg-3)가 매칭된 경우 전개도 단면표시: "A (Seg-3)"
    ;;     - 단면기호만 작도하며 지반 주상도는 추가 생성하지 않는다.
    (if (and opp_seg_id opp_x opp_ref_cy)
      (progn
        (setq cur_seg_data (nth (1- opp_seg_id) seg-list))
        (setq cur_emb_m (drg-extract-embed-from-zone cur_seg_data cur_seg_data))
        (setq top_box_y (+ opp_ref_cy (* 1800.0 s_factor_sec)))
        (setq bot_pile_y (get-y-on-pline ent_excav opp_x))
        (if bot_pile_y
          (setq bot_pile_y (- bot_pile_y (* cur_emb_m 1000.0)))
          (setq bot_pile_y (- opp_ref_cy 10000.0))
        )
        (setq sec_mark_ents
          (append sec_mark_ents
            (vl-catch-all-apply 'drg-draw-elevation-section-mark
              (list opp_x top_box_y bot_pile_y (strcat letter_str " (Seg-" (itoa opp_seg_id) ")")))
          )
        )
        (if (vl-catch-all-error-p (last sec_mark_ents))
          (princ (strcat "\n[단면진단] 전개도 단면기호(맞은편) 실패 Seg-" (itoa opp_seg_id) " : "
                         (vl-catch-all-error-message (last sec_mark_ents))))
        )
      )
    )

    ;; [3] 평면도 단면표시 (관통 단면이면 pt1_plan ~ pt2_plan 양단에 "A"-"A", 단독 단면이면 pt1_plan 전후에 "A"-"A")
    (if pt1_plan
      (setq sec_mark_ents
        (append sec_mark_ents
          (vl-catch-all-apply 'drg-draw-plan-section-mark
            (list pt1_plan pt2_plan in_norm1 letter_str))
        )
      )
    )
    (if (and sec_mark_ents (vl-catch-all-error-p (last sec_mark_ents)))
      (princ (strcat "\n[단면진단] 평면도 단면기호 실패 " letter_str " : "
                     (vl-catch-all-error-message (last sec_mark_ents))
                     " / pt1=" (vl-princ-to-string pt1_plan)
                     " pt2=" (vl-princ-to-string pt2_plan)
                     " in_norm=" (vl-princ-to-string in_norm1)))
    )
  )
  (princ (strcat "\n-> 전개도/평면도 '단면표시' 기호 작도 개수: " (itoa (length section_pair_list)) "개소"))

  ;; 8. 결과 리포트 출력
  (princ "\n\n========================================================")
  (princ (strcat "\n[TSP -> DRG 전개도 자동작도 완료] (총 스팬 수: " (itoa (length tsp_spans)) "개 구간)"))
  (princ (strcat "\n-> TSP 모서리 기준 상단 번호 생성 개수: " (itoa (length corner_info_list)) "개소"))
  (princ (strcat "\n-> 전개도에 작도된 총 측면말뚝 개수: " (itoa (length final_piles)) "개"))
  (princ (strcat "\n-> 전개도에 배치된 지보재 블록 개수: " (itoa (length block_ents)) "개"))
  (princ (strcat "\n-> 전개도에 배치된 지반 주상도 블록 개수: " (itoa (length soil_col_ents)) "개"))
  (if (and ss_plan plan_capsules)
    (progn
      (setq max_cap_num 0)
      (foreach pc plan_capsules
        (foreach cap_s (cadr pc)
          (if (> (atoi cap_s) max_cap_num) (setq max_cap_num (atoi cap_s)))
        )
      )
      (princ (strcat "\n-> 평면도에서 인식된 마지막 캡슐 숫자: " (itoa max_cap_num)))
      (if (/= (length final_piles) max_cap_num)
        (princ "\n주의!!! 전개도에 작도된 측면말뚝 개수와 캡슐 내 숫자가 불일치합니다.")
        (princ "\n완벽합니다! 전개도 작도 및 캡슐 검증이 정확히 일치합니다.")
      )
    )
  )
  (princ "\n========================================================\n")
  (princ)
)
