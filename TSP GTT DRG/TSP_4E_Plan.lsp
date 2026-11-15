;;; ==========================================================================
;;; 파일명: TSP_4E_Plan.lsp
;;; 구  분: 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part E)
;;; 역  할: 평면 띠장 오프셋/상향, H-Pile/토류판 배치, 사보강재/화타쐐기, 차수그라우팅, 연속 CIP 평면 배열, 세그먼트/모서리 번호표 및 수동말뚝 삭제 [tsp-draw-all-corner-braces, valid-hit]
;;; 포함 함수: tsp-draw-all-corner-braces, valid-hit
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-draw-all-corner-braces : 사보강재(Corner Strut) 및 다중 대칭 보강재(Brace Array) 작도 통합 모듈
;;;;==========================================================================
(defun tsp-draw-all-corner-braces (boundary-ent boundary-orient / boundary-handle is-closed orig-vertices num-vertices max-segments idx prev-seg-idx next-seg-idx prev-def next-def prev-vertex curr-vertex next-vertex p-brace c-upg upg-vals prev-wale-spec prev-wale-vals w-h-prev next-wale-spec next-wale-vals w-h-next dir-A dir-B norm-A norm-B brace-b-rows b-row-idx b-spec b-custom b-dist-m b-jack b-jack-side b-conn b-dist b-h half-w pt-A-base pt-B-base pt-A-inner pt-B-inner brace-dir L1-A L1-B L2-A L2-B pA1 pA2 pB1 pB2 conn_pts_A conn_pts_B brace_A1 brace_A2 brace_B1 brace_B2 jack-side jack-len pt-jack-ins jack-rot brace_A1_in brace_A2_in brace_B1_in brace_B2_in b-tw half-tw mid_A_in mid_B_in web1_A web1_B web2_A web2_B max_brace_len brace_boundaries stiff-h stiff-tw pt_A_face_p1 pt_B_face_p1 pt-inner-corner vec_A vec_B vec_sum stiff-dir half-ang brace_len slender-limit is-manual manual-interval stiff-enable stiff-margin L-max total-L num-div interval cur-off offsets off axis-p1 axis-p2 p_out_L_line p_out_R_line pA_inf1 pA_inf2 pB_inf1 pB_inf2 hit-L_A hit-L_B hit-R_A hit-R_B hit-L hit-R proj-L proj-R max-proj hit-C_A hit-C_B hit-C p_flat_center p_flat_L p_flat_R p_stiff_base_center p_stiff_base_L p_stiff_base_R p_start_center p_out_L p_out_R p_in_L p_in_R max_dist_overall p_x1 p_x2 d1 d2 current_d merged_intervals sorted last_m pt_from pt_to p_end_L p_end_R valid-hit intervals cur_int hit-A hit-B hit-Wale wall-norm wl_dir ang_diff flat-half-w p_w1 p_w2 p_f1 p_f2 p_fL p_fR d_foot p_foot_A p_foot_B p_flat_A p_flat_B p_stiff_base_A p_stiff_base_B is-direct-A is-direct-B is-conn-direct conn-A conn-B)

  (defun valid-hit (hit p-cor dir)
    (if hit (if (> (tsp-project-dist p-cor dir hit) -1e-3) hit nil) nil)
  )

  ;; 객체의 고유 핸들을 추출하여 변수에 저장
  (setq boundary-handle (tsp-get-entity-handle boundary-ent))

  (setq is-closed (is-closed-polyline boundary-ent))
  (setq orig-vertices (mapcar '(lambda (item) (cdr (assoc 'V-START item))) *segment-list*))
  (if (not is-closed) (setq orig-vertices (append orig-vertices (list (cdr (assoc 'V-END (last *segment-list*)))))))
  (setq num-vertices (length orig-vertices))
  (setq max-segments (if is-closed num-vertices (1- num-vertices)))

  (setq idx 0)
  (while (< idx num-vertices)
    (setq prev-seg-idx (if (= idx 0) (if is-closed (1- num-vertices) -1) (1- idx)))
    (setq next-seg-idx (if (>= idx max-segments) -1 idx))
    (setq prev-def (if (>= prev-seg-idx 0) (cdr (assoc 'IS-DEFINED (nth prev-seg-idx *segment-list*))) nil))
    (setq next-def (if (>= next-seg-idx 0) (cdr (assoc 'IS-DEFINED (nth next-seg-idx *segment-list*))) nil))

    (if (and prev-def next-def)
      (progn
        (setq prev-vertex (nth prev-seg-idx orig-vertices) curr-vertex (nth idx orig-vertices) next-vertex (nth (if is-closed (if (= idx (1- num-vertices)) 0 (1+ idx)) (1+ idx)) orig-vertices))
        (setq p-brace nil)
        (if (and (>= idx 0) (< idx (length *segment-list*))) (setq p-brace (cdr (assoc 'CORNER-BRACE (nth idx *segment-list*)))))

                (if (and p-brace (= (car p-brace) "Y"))
          (progn
            ;; [다단 사보강재 평면 통합 (안 2)]
            ;; - 모든 단이 동일한 설정이면 1개만 작도
            ;; - 깊이별로 다른 설정의 사보강재가 있으면 모든 이격거리를 통합하되,
            ;;   동일 이격거리(오차 10mm 이내)에 겹치는 열은 강성이 큰 규격(2H 우선, 단면 H 큰 것 우선)으로 병합
            (setq brace-b-rows (cadr p-brace))
            (setq slender-limit (if (> (length p-brace) 2) (nth 2 p-brace) "1"))
            (setq is-manual (if (> (length p-brace) 3) (nth 3 p-brace) "0"))
            (setq manual-interval (if (> (length p-brace) 4) (nth 4 p-brace) "0.0"))
            (setq stiff-enable (if (> (length p-brace) 5) (nth 5 p-brace) "1"))
            (setq stiff-margin (if (> (length p-brace) 6) (atof (nth 6 p-brace)) 50.0))

            (if (and (> (length p-brace) 12) (= (nth 11 p-brace) "MULTI") (listp (nth 12 p-brace)))
              (progn
                (setq merged-rows '())
                (foreach st (nth 12 p-brace)
                  (if (= (nth 6 st) "1") (setq stiff-enable "1"))
                  (if (= (nth 3 st) "0") (setq slender-limit "0"))
                  (foreach r (nth 2 st)
                    (setq r-dist (float (nth 2 r)))
                    (setq r-score (+ (if (wcmatch (nth 0 r) "2H *") 100000 0) (* (nth 0 (nth 1 r)) 100) (nth 1 (nth 1 r))))
                    (setq exist-r (vl-some '(lambda (er) (if (< (abs (- (float (nth 2 er)) r-dist)) 0.01) er nil)) merged-rows))
                                        (if exist-r
                      (progn
                        (setq ex-score (+ (if (wcmatch (nth 0 exist-r) "2H *") 100000 0) (* (nth 0 (nth 1 exist-r)) 100) (nth 1 (nth 1 exist-r))))
                        (if (or (> r-score ex-score)
                                (and (= r-score ex-score)
                                     (or (<= (length exist-r) 5) (= (nth 5 exist-r) "AUTO") (null (nth 5 exist-r)))
                                     (> (length r) 5)
                                     (nth 5 r)
                                     (/= (nth 5 r) "AUTO")))
                          (setq merged-rows (subst r exist-r merged-rows))
                        )
                      )
                      (setq merged-rows (append merged-rows (list r)))
                    )
                  )
                )
                (setq brace-b-rows (vl-sort merged-rows '(lambda (a b) (< (float (nth 2 a)) (float (nth 2 b))))))
              )
            )

            (setq c-upg (cdr (assoc 'UPGRADE-WALE (nth idx *segment-list*))))
            (setq upg-vals (if (and c-upg (= (car c-upg) "Y")) (parse-h-spec (cadr c-upg)) nil))

            ;; 띠장(Wale) 오프셋 작도 로직과 완벽히 동기화되도록 '기본 띠장', '지보재 설정 띠장', '상향 띠장' 중 최대값(max)을 적용
            (setq prev-seg-obj (nth prev-seg-idx *segment-list*))
            (setq prev-wale-spec (cdr (assoc 'WALE-SPEC prev-seg-obj)))
            (setq prev-wale-vals (if (= prev-wale-spec "User-defined") (cdr (assoc 'WALE-CUSTOM prev-seg-obj)) (parse-h-spec prev-wale-spec)))
            (setq base-h-prev (if prev-wale-vals (nth 0 prev-wale-vals) 300))
            (if (cdr (assoc 'SUPPORT-LIST prev-seg-obj))
              (foreach s-it (cdr (assoc 'SUPPORT-LIST prev-seg-obj))
                (setq s-iv (tsp-get-support-item-dist-interval s-it (cdr (assoc 'V-START prev-seg-obj)) (cdr (assoc 'ANGLE prev-seg-obj)) (cdr (assoc 'LENGTH prev-seg-obj))))
                (if (>= (cdr s-iv) (- (cdr (assoc 'LENGTH prev-seg-obj)) 3000.0))
                  (progn
                    (setq s-wv (tsp-get-support-item-wale-vals s-it prev-wale-spec (cdr (assoc 'WALE-CUSTOM prev-seg-obj))))
                    (if (> (nth 0 s-wv) base-h-prev) (setq base-h-prev (nth 0 s-wv)))
                  )
                )
              )
            )
            (setq w-h-prev (if upg-vals (max (nth 0 upg-vals) base-h-prev) base-h-prev))

            (setq next-seg-obj (nth next-seg-idx *segment-list*))
            (setq next-wale-spec (cdr (assoc 'WALE-SPEC next-seg-obj)))
            (setq next-wale-vals (if (= next-wale-spec "User-defined") (cdr (assoc 'WALE-CUSTOM next-seg-obj)) (parse-h-spec next-wale-spec)))
            (setq base-h-next (if next-wale-vals (nth 0 next-wale-vals) 300))
            (if (cdr (assoc 'SUPPORT-LIST next-seg-obj))
              (foreach s-it (cdr (assoc 'SUPPORT-LIST next-seg-obj))
                (setq s-iv (tsp-get-support-item-dist-interval s-it (cdr (assoc 'V-START next-seg-obj)) (cdr (assoc 'ANGLE next-seg-obj)) (cdr (assoc 'LENGTH next-seg-obj))))
                (if (<= (car s-iv) 3000.0)
                  (progn
                    (setq s-wv (tsp-get-support-item-wale-vals s-it next-wale-spec (cdr (assoc 'WALE-CUSTOM next-seg-obj))))
                    (if (> (nth 0 s-wv) base-h-next) (setq base-h-next (nth 0 s-wv)))
                  )
                )
              )
            )
            (setq w-h-next (if upg-vals (max (nth 0 upg-vals) base-h-next) base-h-next))
            ;; create-wale-offsets의 eff-h 계산과 완벽히 동기화되도록 사보강재 코너에서는 항상 양쪽 최대 띠장 폭으로 통일
            (setq w-h-prev (max w-h-prev w-h-next)
                  w-h-next w-h-prev)

            (setq dir-A (angle curr-vertex prev-vertex) dir-B (angle curr-vertex next-vertex))
            (setq norm-A (+ (angle prev-vertex curr-vertex) (* boundary-orient (/ pi 2.0))))
            (setq norm-B (+ (angle curr-vertex next-vertex) (* boundary-orient (/ pi 2.0))))

            (setq b-row-idx 0 max_brace_len 0.0 brace_boundaries '() stiff-h 300.0 stiff-tw 10.0 brace-row-data-list '())

            (setq pt_A_face_p1 (polar curr-vertex norm-A w-h-prev))
            (setq pt_B_face_p1 (polar curr-vertex norm-B w-h-next))
            (setq pt-inner-corner (inters pt_A_face_p1 (polar pt_A_face_p1 dir-A 100.0) pt_B_face_p1 (polar pt_B_face_p1 dir-B 100.0) nil))
            (if (null pt-inner-corner) (setq pt-inner-corner curr-vertex))

            ;; 굴착면 안쪽을 향하도록 법선(norm) 벡터를 합산
            (setq vec_nA (list (cos norm-A) (sin norm-A)))
            (setq vec_nB (list (cos norm-B) (sin norm-B)))
            (setq vec_n_sum (list (+ (car vec_nA) (car vec_nB)) (+ (cadr vec_nA) (cadr vec_nB))))
            (setq stiff-dir (angle '(0 0 0) vec_n_sum))

            ;; 모서리 내각(delta)의 절반을 삼각함수로 안전하게 산출
            (setq delta (abs (- dir-A dir-B)))
            (if (> delta pi) (setq delta (- (* 2.0 pi) delta)))
            (setq half-ang (/ delta 2.0))

            (create-strut-jack-block) (create-strut-screw-jack-block)
            (if (not (tblsearch "APPID" "TSP_STRUT_SPEC")) (regapp "TSP_STRUT_SPEC"))

                            ;;;; 사보강재 단일/맞버팀대(Double Strut) 작도 로직
              (foreach b-row brace-b-rows
                (setq b-spec (nth 0 b-row) b-custom (nth 1 b-row) b-dist-m (nth 2 b-row) b-jack (nth 3 b-row) b-conn (nth 4 b-row))
                (setq b-jack-side (if (> (length b-row) 5) (nth 5 b-row) "AUTO"))
                (if (or (null b-jack-side) (= b-jack-side "")) (setq b-jack-side "AUTO"))
                (if (or (null b-jack) (= b-jack ""))
                  (setq b-jack (if (= b-row-idx 0) "잭 없음" "스크류잭"))
                )
                (if (null b-conn) (setq b-conn (if (= b-row-idx 0) "화타쐐기 없음" "화타쐐기 TYPE-G1")))

                (setq b-dist (* b-dist-m 1000.0) b-h (nth 0 b-custom) b-width (nth 1 b-custom) half-w (/ b-width 2.0) stiff-h b-h stiff-tw (nth 2 b-custom))

                (setq pt-A-base (polar curr-vertex dir-A b-dist) pt-B-base (polar curr-vertex dir-B b-dist))
                (setq pt-A-inner (list (car (polar pt-A-base norm-A w-h-prev)) (cadr (polar pt-A-base norm-A w-h-prev)) 0.0))
                (setq pt-B-inner (list (car (polar pt-B-base norm-B w-h-next)) (cadr (polar pt-B-base norm-B w-h-next)) 0.0))

                (create-layer-if-not-exists "_mpoint" "2")
                (entmake (list '(0 . "POINT") '(100 . "AcDbEntity") '(100 . "AcDbPoint") (cons 8 "_mpoint") (cons 62 2) (list 10 (car pt-A-inner) (cadr pt-A-inner) 0.0)))
                (entmake (list '(0 . "POINT") '(100 . "AcDbEntity") '(100 . "AcDbPoint") (cons 8 "_mpoint") (cons 62 2) (list 10 (car pt-B-inner) (cadr pt-B-inner) 0.0)))

                (setq brace-dir (angle pt-A-inner pt-B-inner))
                (setq is-double (wcmatch b-spec "2H *"))
                (setq strut-offsets (if is-double (list b-width (- b-width)) (list 0.0)))

                (setq sub-idx 0)
                (foreach s-off strut-offsets
                  (setq off-dir (+ brace-dir (/ pi 2.0)))
                  (setq p-off-A (polar pt-A-inner off-dir s-off))
                  (setq p-off-B (polar pt-B-inner off-dir s-off))

                  (setq sub-pt-A-inner (inters pt-A-inner (polar pt-A-inner dir-A 100.0) p-off-A p-off-B nil))
                  (setq sub-pt-B-inner (inters pt-B-inner (polar pt-B-inner dir-B 100.0) p-off-A p-off-B nil))

                  (if (and sub-pt-A-inner sub-pt-B-inner)
                    (progn
                      (setq sub-brace_len (distance sub-pt-A-inner sub-pt-B-inner))
                      (if (> sub-brace_len max_brace_len) (setq max_brace_len sub-brace_len))

                      (setq sub-brace-dir (angle sub-pt-A-inner sub-pt-B-inner))
                      (setq L1-A (polar sub-pt-A-inner (+ sub-brace-dir (/ pi 2.0)) half-w) L1-B (polar sub-pt-B-inner (+ sub-brace-dir (/ pi 2.0)) half-w))
                      (setq L2-A (polar sub-pt-A-inner (- sub-brace-dir (/ pi 2.0)) half-w) L2-B (polar sub-pt-B-inner (- sub-brace-dir (/ pi 2.0)) half-w))

                      (setq pA1 (inters sub-pt-A-inner (polar sub-pt-A-inner dir-A 100.0) L1-A L1-B nil))
                      (setq pA2 (inters sub-pt-A-inner (polar sub-pt-A-inner dir-A 100.0) L2-A L2-B nil))
                      (setq pB1 (inters sub-pt-B-inner (polar sub-pt-B-inner dir-B 100.0) L1-A L1-B nil))
                      (setq pB2 (inters sub-pt-B-inner (polar sub-pt-B-inner dir-B 100.0) L2-A L2-B nil))

                      (create-layer-if-not-exists "_사보강재(Corner_Strut)" "3") (create-layer-if-not-exists "_화타쐐기(Wedge)" "2")

                      (if (and pA1 pB1 pA2 pB2)
                        (progn
                                                    ;; 맞버팀대(2H) 및 단일(1H) 단부 잭 위치 결정 ("잭 없음" 선택 시 jack-side = nil, 그 외 사용자 지정 A/B/REV/AUTO 반영)
                          (if (= b-jack "잭 없음")
                            (setq jack-side nil)
                            (cond
                              ((= b-jack-side "A") (setq jack-side "A"))
                              ((= b-jack-side "B") (setq jack-side "B"))
                              ((= b-jack-side "REV")
                               (if is-double
                                 (setq jack-side (if (= sub-idx 0) "B" "A"))
                                 (setq jack-side (if (= (rem b-row-idx 2) 1) "B" "A"))
                               )
                              )
                              (t
                               (if is-double
                                 (setq jack-side (if (= sub-idx 0) "A" "B"))
                                 (setq jack-side (if (= (rem b-row-idx 2) 1) "A" "B"))
                               )
                              )
                            )
                          )

                          ;; [마감철판 직결] 잭이 붙는 단부는 "화타쐐기 없음"과 동일하게, 잭이 없는 단부만 직결 로직 적용
                          (setq is-conn-direct (or (null b-conn) (= b-conn "") (= b-conn "마감철판 직결")))
                          (setq conn-A (if (and is-conn-direct (= jack-side "A")) "화타쐐기 없음" b-conn))
                          (setq conn-B (if (and is-conn-direct (= jack-side "B")) "화타쐐기 없음" b-conn))

                          (setq conn_pts_A (tsp-draw-wedge-connection sub-pt-A-inner pA1 pA2 sub-brace-dir dir-A norm-A conn-A))
                          (setq brace_A1 (car conn_pts_A) brace_A2 (cadr conn_pts_A) wedge_segs_A (caddr conn_pts_A))

                          (setq conn_pts_B (tsp-draw-wedge-connection sub-pt-B-inner pB1 pB2 (+ sub-brace-dir pi) dir-B norm-B conn-B))
                          (setq brace_B1 (car conn_pts_B) brace_B2 (cadr conn_pts_B) wedge_segs_B (caddr conn_pts_B))

                          (setq jack-len 583.0)
                          (if (= jack-side "A")
                            (progn
                              (setq pt-jack-ins (list (/ (+ (car brace_A1) (car brace_A2)) 2.0) (/ (+ (cadr brace_A1) (cadr brace_A2)) 2.0) 0.0) jack-rot sub-brace-dir)
                              (entmake (list '(0 . "INSERT") (cons 2 b-jack) (cons 8 (if (= b-jack "스크류잭") "_스크류잭" "_유압잭")) (cons 62 2) (cons 10 pt-jack-ins) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 jack-rot)))
                              (setq brace_A1 (polar brace_A1 sub-brace-dir jack-len) brace_A2 (polar brace_A2 sub-brace-dir jack-len))
                            )
                          )
                          (if (= jack-side "B")
                            (progn
                              (setq pt-jack-ins (list (/ (+ (car brace_B1) (car brace_B2)) 2.0) (/ (+ (cadr brace_B1) (cadr brace_B2)) 2.0) 0.0) jack-rot (+ sub-brace-dir pi))
                              (entmake (list '(0 . "INSERT") (cons 2 b-jack) (cons 8 (if (= b-jack "스크류잭") "_스크류잭" "_유압잭")) (cons 62 2) (cons 10 pt-jack-ins) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 jack-rot)))
                              (setq brace_B1 (polar brace_B1 (+ sub-brace-dir pi) jack-len) brace_B2 (polar brace_B2 (+ sub-brace-dir pi) jack-len))
                                    )
                                  )

                                  (setq cur-seg-id (cdr (assoc 'ID (nth idx *segment-list*))))
                                  (setq outer-margin (* 2.0 (+ (if is-double b-width 0.0) half-w 100.0)))
                                  (setq pA_bound (polar pt-A-inner dir-A outer-margin))
                                  (setq pB_bound (polar pt-B-inner dir-B outer-margin))

                                  (setq strut-xdata
                                    (list -3
                                      (list "TSP_STRUT_SPEC"
                                        (cons 1070 cur-seg-id)
                                        (cons 1040 (float b-h))
                                        (cons 1040 (float b-width))
                                        (cons 1040 (float (nth 2 b-custom)))
                                        (cons 1040 (float (nth 3 b-custom)))
                                        (cons 1010 pt-inner-corner)
                                        (cons 1010 pA_bound)
                                        (cons 1010 pB_bound)
                                        (cons 1000 boundary-handle)
                                      )
                                    )
                                  )

                                  ;; [마감철판 직결] 잭이 없는 단부는 t14 연결재 없이 사보강재 몸통이 마감철판에 바로 직결
                                  (setq is-direct-A (and (or (null b-conn) (= b-conn "") (= b-conn "마감철판 직결")) (/= jack-side "A")))
                                  (setq is-direct-B (and (or (null b-conn) (= b-conn "") (= b-conn "마감철판 직결")) (/= jack-side "B")))

                                  ;; t14 마감 철판용 안쪽 좌표 산출 (직결 단부는 오프셋 0)
                                  (setq brace_A1_in (polar brace_A1 sub-brace-dir (if is-direct-A 0.0 14.0)) brace_A2_in (polar brace_A2 sub-brace-dir (if is-direct-A 0.0 14.0)))
                                  (setq brace_B1_in (polar brace_B1 (+ sub-brace-dir pi) (if is-direct-B 0.0 14.0)) brace_B2_in (polar brace_B2 (+ sub-brace-dir pi) (if is-direct-B 0.0 14.0)))

                                  ;; 사보강재 시작단(A) t14 마감 철판 (직결 단부는 생략)
                                  (if (not is-direct-A) (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_사보강재(Corner_Strut)") '(62 . 3) '(90 . 4) '(70 . 1)
                                                         (list 10 (car brace_A1) (cadr brace_A1)) (list 10 (car brace_A2) (cadr brace_A2))
                                                         (list 10 (car brace_A2_in) (cadr brace_A2_in)) (list 10 (car brace_A1_in) (cadr brace_A1_in)))
                                                   (list strut-xdata))))

                                  ;; 사보강재 끝단(B) t14 마감 철판 (직결 단부는 생략)
                                  (if (not is-direct-B) (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_사보강재(Corner_Strut)") '(62 . 3) '(90 . 4) '(70 . 1)
                                                         (list 10 (car brace_B1) (cadr brace_B1)) (list 10 (car brace_B2) (cadr brace_B2))
                                                         (list 10 (car brace_B2_in) (cadr brace_B2_in)) (list 10 (car brace_B1_in) (cadr brace_B1_in)))
                                                   (list strut-xdata))))

                                  ;; 사보강재 메인 몸통
                                  (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_사보강재(Corner_Strut)") '(62 . 3) '(90 . 4) '(70 . 1)
                                                         (list 10 (car brace_A1_in) (cadr brace_A1_in)) (list 10 (car brace_A2_in) (cadr brace_A2_in))
                                                         (list 10 (car brace_B2_in) (cadr brace_B2_in)) (list 10 (car brace_B1_in) (cadr brace_B1_in)))
                                                   (list strut-xdata)))

(setq half-tw (/ (nth 2 b-custom) 2.0))
(setq mid_A_in (list (/ (+ (car brace_A1_in) (car brace_A2_in)) 2.0) (/ (+ (cadr brace_A1_in) (cadr brace_A2_in)) 2.0) 0.0))
(setq mid_B_in (list (/ (+ (car brace_B1_in) (car brace_B2_in)) 2.0) (/ (+ (cadr brace_B1_in) (cadr brace_B2_in)) 2.0) 0.0))
(setq web1_A (polar mid_A_in (+ sub-brace-dir (/ pi 2.0)) half-tw) web1_B (polar mid_B_in (+ sub-brace-dir (/ pi 2.0)) half-tw))
(setq web2_A (polar mid_A_in (- sub-brace-dir (/ pi 2.0)) half-tw) web2_B (polar mid_B_in (- sub-brace-dir (/ pi 2.0)) half-tw))
;; [마감철판 직결] 웹 파선을 마감철판 윗면(사선 단부)까지 연장
(if is-direct-A
  (progn
    (setq web1_A (cond ((inters web1_A web1_B brace_A1 brace_A2 nil)) (web1_A)))
    (setq web2_A (cond ((inters web2_A web2_B brace_A1 brace_A2 nil)) (web2_A)))
  )
)
(if is-direct-B
  (progn
    (setq web1_B (cond ((inters web1_A web1_B brace_B1 brace_B2 nil)) (web1_B)))
    (setq web2_B (cond ((inters web2_A web2_B brace_B1 brace_B2 nil)) (web2_B)))
  )
)
(if (not (tblsearch "LTYPE" "DASHED")) (vl-catch-all-apply 'vla-load (list (vla-get-linetypes (vla-get-activedocument (vlax-get-acad-object))) "DASHED" "acad.lin")))
(entmake
  (append
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      (cons 8 "_사보강재(Corner_Strut)")
      (cons 62 1)
      (cons 6 "DASHED")
      (cons 48 (* (if (and (boundp '*tsp-scale-plan*) *tsp-scale-plan*) *tsp-scale-plan* 600.0) (/ 500.0 600.0) (if (= (getvar "MEASUREMENT") 1) (/ 1.0 25.0) 1.0)))
      '(90 . 2)
      '(70 . 0)
      (list 10 (car web1_A) (cadr web1_A))
      (list 10 (car web1_B) (cadr web1_B))
    )
    (list strut-xdata)
  )
)
(entmake
  (append
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      (cons 8 "_사보강재(Corner_Strut)")
      (cons 62 1)
      (cons 6 "DASHED")
      (cons 48 (* (if (and (boundp '*tsp-scale-plan*) *tsp-scale-plan*) *tsp-scale-plan* 600.0) (/ 500.0 600.0) (if (= (getvar "MEASUREMENT") 1) (/ 1.0 25.0) 1.0)))
      '(90 . 2)
      '(70 . 0)
      (list 10 (car web2_A) (cadr web2_A))
      (list 10 (car web2_B) (cadr web2_B))
    )
    (list strut-xdata)
  )
)

                          (setq brace_boundaries (append brace_boundaries
                            (list
                              (cons "WEDGE" (list (list brace_A1 brace_B1) (list brace_B1 brace_B2) (list brace_B2 brace_A2) (list brace_A2 brace_A1)))
                              (cons "WEDGE" wedge_segs_A)
                              (cons "WEDGE" wedge_segs_B)
                                )))
                        )
                      )
                    )
                  )
                  (setq sub-idx (1+ sub-idx))
                )

                ;; 현재 사보강재 열의 핵심 데이터(중심선, 방향, 맞버팀대 여부, 폭, 규격)를 리스트에 보존
                (setq brace-row-data-list (append brace-row-data-list (list (list pt-A-inner pt-B-inner brace-dir is-double b-width half-w b-custom))))

                (setq b-row-idx (1+ b-row-idx))
              )

            ;; [3] 다중 대칭 보강재(Brace Array) 작도 및 동적 간섭/절단 엔진 (옵션에 따라 통제됨)
            (if (and (> (length brace-b-rows) 0) (= stiff-enable "1"))
              (progn
                (setq L-max (tsp-calc-unsupported-length (nth 1 (last brace-b-rows)) (if (= slender-limit "0") 100.0 120.0)))
                (setq total-L (/ max_brace_len 1000.0))

                (setq offsets '())
                (if (> total-L L-max)
                  (progn
                    (create-layer-if-not-exists "_보강재(Brace)" "7")
                   (if (= is-manual "1")
                      (progn
                        (setq interval (* (atof manual-interval) 1000.0))
                        (if (<= interval 0.01)
                          (setq offsets (list 0.0))
                          (progn
                            (setq cur-off interval offsets (list 0.0))
                            (while (<= cur-off (/ max_brace_len 2.0))
                              (setq offsets (append offsets (list cur-off (- cur-off))) cur-off (+ cur-off interval))
                            )
                          )
                        )
                      )
                      (progn
                        (setq num-div (fix (+ (/ total-L L-max) 0.9999)) interval (/ max_brace_len num-div) i 1)
                        (while (< i num-div)
                          (setq off (- (* i interval) (/ max_brace_len 2.0)) offsets (append offsets (list off)) i (1+ i))
                        )
                      )
                    )
                  )
                )

                (setq pA_inf1 pt-inner-corner pA_inf2 (polar pt-inner-corner dir-A 100.0))
                (setq pB_inf1 pt-inner-corner pB_inf2 (polar pt-inner-corner dir-B 100.0))
                (setq stiff-half-w (/ stiff-h 2.0))

               ;; 중간말뚝 옵션 읽기 및 좌/우 설정 초기화
                (setq post-enable (if (> (length p-brace) 7) (nth 7 p-brace) "1"))
                (setq post-side (if (> (length p-brace) 8) (nth 8 p-brace) "L"))
                ;; 9번째 인덱스의 배치 모드 (0=전체, 1=교차)
                (setq post-layout (if (> (length p-brace) 9) (nth 9 p-brace) "1"))
                ;; 10번째 인덱스의 교차 반전 모드 (0=기본, 1=반전)
                (setq post-invert (if (> (length p-brace) 10) (nth 10 p-brace) "0"))

                (setq stiff-idx 0) ;; 보강재(가로축) 인덱스 초기화
                (foreach off offsets
                  (setq draw-this-brace nil)
                  (setq hit-Wale nil)

                  (if (equal off 0.0 1e-4)
                    (progn
                      (setq d_foot (/ stiff-half-w (sin half-ang)))
                      (setq p_start_center (polar pt-inner-corner stiff-dir (+ (* d_foot (cos half-ang)) 28.0)))
                      (setq draw-this-brace T)
                    )
                    (progn
                      (setq axis-p1 (polar pt-inner-corner brace-dir off) axis-p2 (polar axis-p1 stiff-dir 100.0))
                      (setq hit-A (inters axis-p1 axis-p2 pA_inf1 pA_inf2 nil))
                      (setq hit-B (inters axis-p1 axis-p2 pB_inf1 pB_inf2 nil))

                      (setq hit-Wale nil wall-norm nil wl_dir nil)
                      (if (valid-hit hit-A pt-inner-corner dir-A) (setq hit-Wale hit-A wall-norm norm-A wl_dir dir-A)
                        (if (valid-hit hit-B pt-inner-corner dir-B) (setq hit-Wale hit-B wall-norm norm-B wl_dir dir-B))
                      )

                      (if hit-Wale
                        (progn
                          (setq p_bp_center hit-Wale)
                          (setq p_ep_center (polar p_bp_center stiff-dir 220.0))
                          (setq p_start_center (polar p_ep_center stiff-dir 14.0))
                          (setq draw-this-brace T)
                        )
                      )
                    )
                  )

                 (if draw-this-brace
                    (progn
                      (if (= post-enable "1")
                        (progn
                          (create-layer-if-not-exists "_중간말뚝(Post pile)" "3")

                          (setq row-idx 0) ;; 사보강재 열(세로축) 인덱스 초기화
                          (foreach br-data brace-row-data-list
                            (setq cur-pt-A-inner (list (car (nth 0 br-data)) (cadr (nth 0 br-data)) 0.0)
                                  cur-pt-B-inner (list (car (nth 1 br-data)) (cadr (nth 1 br-data)) 0.0)
                                  cur-brace-dir  (nth 2 br-data)
                                  cur-is-double  (nth 3 br-data)
                                  cur-b-width    (nth 4 br-data)
                                  cur-half-w     (nth 5 br-data)
                                  cur-b-custom   (nth 6 br-data)
                                  cur-h          (nth 0 cur-b-custom)
                                  cur-b          (nth 1 cur-b-custom)
                                  cur-tw         (nth 2 cur-b-custom)
                                  cur-tf         (nth 3 cur-b-custom))

                            ;; 동적 말뚝 블록 생성
                            (setq post-block (create-post-pile-block cur-h cur-b cur-tw cur-tf))

                            (setq p_stiff_2d (list (car p_start_center) (cadr p_start_center) 0.0))
                            (setq p_stiff_end (polar p_stiff_2d stiff-dir 1000.0))

                            (setq cross-pt (inters cur-pt-A-inner cur-pt-B-inner p_stiff_2d p_stiff_end nil))

                           (if cross-pt
                              (progn
                                ;; 현재 사보강재 열의 길이 및 유효성 플래그 초기화
                                (setq cur-brace-len (distance cur-pt-A-inner cur-pt-B-inner))
                                (setq is-valid-post T)

                                ;; 1. 세장비 초과 검사 (최대거리를 초과하는 경우에만 생성)
                                (if (<= cur-brace-len (* L-max 1000.0))
                                  (progn
                                    (setq is-valid-post nil)
                                  )
                                )

                                ;; 2. 띠장 간섭 및 최소 이격거리 확보 (+ 300.0 cur-h 연동)
                                (if (and is-valid-post (<= (tsp-project-dist p_start_center stiff-dir cross-pt) (+ 300.0 cur-h)))
                                  (progn
                                    (setq is-valid-post nil)
                                  )
                                )

                                ;; 3. 교차(지그재그) 배치 검사 및 반전(Invert) 적용
                                (if (and is-valid-post (= post-layout "1"))
                                  (progn
                                    (setq spatial-idx 0)
                                    (if (> interval 0.01)
                                      (setq spatial-idx (fix (if (> off 0) (+ (/ off interval) 0.5) (- (/ off interval) 0.5))))
                                    )
                                    (setq invert-val (if (= post-invert "1") 1 0))
                                    ;; 공간 인덱스, 사보강재 열 인덱스, 반전값을 합산하여 홀/짝 판별
                                    (if (/= (rem (+ spatial-idx row-idx invert-val) 2) 0)
                                      (setq is-valid-post nil)
                                    )
                                  )
                                )

                                ;; 조건 만족 시에만 중간말뚝 작도
                                (if is-valid-post
                                  (progn
                                    ;; [1단계] 기본 형태인 '왼쪽(L)' 기준으로 1H/2H 고유 좌표 및 회전각 산출
                                    (if cur-is-double
                                      (progn
                                        ;; 2H: 왼쪽 측면 밀착
                                        (setq pt-post-ins (polar cross-pt (- stiff-dir (/ pi 2.0)) stiff-half-w))
                                        (setq post-rot (+ stiff-dir pi))
                                      )
                                      (progn
                                        ;; 1H: 왼쪽 보강재 외곽선과 사보강재 내측면의 교차점을 통한 역산
                                        (setq stiff-left-p1 (polar p_stiff_2d (- stiff-dir (/ pi 2.0)) stiff-half-w))
                                        (setq stiff-left-p2 (polar p_stiff_end (- stiff-dir (/ pi 2.0)) stiff-half-w))
                                        (setq cs-norm1 (+ cur-brace-dir (/ pi 2.0)))
                                        (setq cs-norm2 (- cur-brace-dir (/ pi 2.0)))
                                        (setq cp1 (polar cross-pt cs-norm1 cur-half-w))
                                        (setq cp2 (polar cross-pt cs-norm2 cur-half-w))
                                        (setq cs-inner-pt (if (< (distance cp1 p_stiff_2d) (distance cp2 p_stiff_2d)) cp1 cp2))
                                        (setq cs-inner-p2 (polar cs-inner-pt cur-brace-dir 100.0))
                                        (setq pt-target (inters stiff-left-p1 stiff-left-p2 cs-inner-pt cs-inner-p2 nil))
                                        (setq post-rot (+ cur-brace-dir (/ pi 2.0)))
                                        (if pt-target
                                          (setq pt-post-ins (polar pt-target post-rot (/ cur-h 2.0)))
                                          (setq pt-post-ins cross-pt) ; 보험용
                                        )
                                      )
                                    )

                                    ;; [2단계]
                                    (setq y_scale 1.0)

                                    ;; [3단계] 사용자 옵션 '오른쪽(R)' 선택 시에만 기하학적 대칭적용
                                    (if (= post-side "R")
                                      (progn
                                        (setq post-rot (- (* 2.0 stiff-dir) post-rot))
                                        (setq dist-to-ins (distance cross-pt pt-post-ins))
                                        (setq ang-to-ins (angle cross-pt pt-post-ins))
                                        (setq mirror-ang (- (* 2.0 stiff-dir) ang-to-ins))
                                        (setq pt-post-ins (polar cross-pt mirror-ang dist-to-ins))
                                        (setq y_scale -1.0)
                                      )
                                    )
                                    (entmake (list '(0 . "INSERT") (cons 2 post-block) (cons 8 "_중간말뚝(Post pile)") (cons 62 256) (cons 10 pt-post-ins) '(41 . 1.0) (cons 42 y_scale) '(43 . 1.0) (cons 50 post-rot)))
                                  )
                                )
                              )
                            )
                            (setq row-idx (1+ row-idx))
                          )
                        )
                      )
                      (setq max_dist_overall 0.0)
                      (foreach b_bounds brace_boundaries
                        (if (= (car b_bounds) "WEDGE")
                          (foreach seg (cdr b_bounds)
                            (setq p_x1 (inters p_start_center (polar p_start_center stiff-dir 1e6) (car seg) (cadr seg) T))
                            (if p_x1
                              (progn
                                (setq d1 (tsp-project-dist p_start_center stiff-dir p_x1))
                                (if (> d1 max_dist_overall) (setq max_dist_overall d1))
                              )
                            )
                          )
                        )
                      )
                      (setq total_L (+ max_dist_overall stiff-margin)) ;; 돌출 길이에 사용자 지정 여유장 변수 할당

                      (if (> total_L 1000.0)
                        (progn
                          (setq p_out_L (polar p_start_center (+ stiff-dir (/ pi 2.0)) stiff-half-w))
                          (setq p_out_R (polar p_start_center (- stiff-dir (/ pi 2.0)) stiff-half-w))
                          (setq p_in_L (polar p_start_center (+ stiff-dir (/ pi 2.0)) (/ stiff-tw 2.0)))
                          (setq p_in_R (polar p_start_center (- stiff-dir (/ pi 2.0)) (/ stiff-tw 2.0)))

                          (if (equal off 0.0 1e-4)
                            (progn
                              (setq p_foot_A (polar pt-inner-corner dir-A d_foot))
                              (setq p_foot_B (polar pt-inner-corner dir-B d_foot))
                              (setq p_flat_A (polar p_foot_A stiff-dir 14.0))
                              (setq p_flat_B (polar p_foot_B stiff-dir 14.0))

                              (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 "_화타쐐기(Wedge)") (cons 62 2) '(90 . 3) '(70 . 0) (list 10 (car p_foot_A) (cadr p_foot_A)) (list 10 (car pt-inner-corner) (cadr pt-inner-corner)) (list 10 (car p_foot_B) (cadr p_foot_B))))
                              (tsp-draw-closed-poly (list p_foot_A p_foot_B p_flat_B p_flat_A) "_화타쐐기(Wedge)" 2)

                              (setq p_stiff_base_A (polar p_flat_A stiff-dir 14.0))
                              (setq p_stiff_base_B (polar p_flat_B stiff-dir 14.0))
                              (tsp-draw-closed-poly (list p_flat_A p_flat_B p_stiff_base_B p_stiff_base_A) "_보강재(Brace)" 256)
                            )
                            (progn
                             (defun draw-trimmed-line (p-s p-e l-name l-color / dist-total pt-dir intervals b-bounds px px1 px2 d1 d2 hits sorted merged cur-d pt-from pt-to last_m seg p-ray-start p-ray-end dx dy perp-dist)
                                (setq dist-total (distance p-s p-e))
                                (if (> dist-total 0.001)
                                  (progn
                                    (setq pt-dir (angle p-s p-e))
                                    (setq intervals '())
                                    (foreach b_bounds brace_boundaries
                                      (setq hits '())
                                      (if (= (car b_bounds) "WEDGE")
                                        (progn
                                          (setq p-ray-start (polar p-s pt-dir -1e6))
                                          (setq p-ray-end (polar p-s pt-dir 1e6))
                                          (foreach seg (cdr b_bounds)
                                            (setq px (inters p-ray-start p-ray-end (car seg) (cadr seg) T))
                                            (if px
                                              (setq hits (append hits (list (tsp-project-dist p-s pt-dir px))))
                                              (progn
                                                (setq dx (- (car (car seg)) (car p-s)))
                                                (setq dy (- (cadr (car seg)) (cadr p-s)))
                                                (setq perp-dist (abs (- (* dx (sin pt-dir)) (* dy (cos pt-dir)))))
                                                (if (< perp-dist 0.1)
                                                  (setq hits (append hits (list (tsp-project-dist p-s pt-dir (car seg))
                                                                                (tsp-project-dist p-s pt-dir (cadr seg)))))
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                      (if (>= (length hits) 1)
                                        (progn
                                          (setq hits (vl-sort hits '<))
                                          (setq d1 (max 0.0 (min dist-total (car hits))))
                                          (setq d2 (max 0.0 (min dist-total (last hits))))
                                          (if (not (equal d1 d2 0.01))
                                            (setq intervals (append intervals (list (cons d1 d2))))
                                          )
                                        )
                                      )
                                    )
                                    (setq merged '())
                                    (if intervals
                                      (progn
                                        (setq sorted (vl-sort intervals (function (lambda (a b) (< (car a) (car b))))))
                                        (setq merged (list (car sorted)))
                                        (foreach cur (cdr sorted)
                                          (setq last_m (last merged))
                                          (if (<= (car cur) (cdr last_m))
                                            (setq merged (subst (cons (car last_m) (max (cdr last_m) (cdr cur))) last_m merged))
                                            (setq merged (append merged (list cur)))
                                          )
                                        )
                                      )
                                    )
                                    (setq cur-d 0.0)
                                    (foreach cur-int merged
                                      (if (> (car cur-int) (+ cur-d 0.01))
                                        (progn
                                          (setq pt-from (polar p-s pt-dir cur-d))
                                          (setq pt-to (polar p-s pt-dir (car cur-int)))
                                          (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                                                         (cons 8 l-name) (cons 62 l-color) '(90 . 2) '(70 . 0)
                                                         (list 10 (car pt-from) (cadr pt-from)) (list 10 (car pt-to) (cadr pt-to))))
                                        )
                                      )
                                      (setq cur-d (max cur-d (cdr cur-int)))
                                    )
                                    (if (< cur-d (- dist-total 0.01))
                                      (progn
                                        (setq pt-from (polar p-s pt-dir cur-d))
                                        (setq pt-to (polar p-s pt-dir dist-total))
                                        (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                                                       (cons 8 l-name) (cons 62 l-color) '(90 . 2) '(70 . 0)
                                                       (list 10 (car pt-from) (cadr pt-from)) (list 10 (car pt-to) (cadr pt-to))))
                                      )
                                    )
                                  )
                                )
                              )

                              (setq p_fill_L (inters p_out_L (polar p_out_L stiff-dir 100.0) p_bp_center (polar p_bp_center wl_dir 100.0) nil))
                              (setq p_fill_R (inters p_out_R (polar p_out_R stiff-dir 100.0) p_bp_center (polar p_bp_center wl_dir 100.0) nil))
                              (if (null p_fill_L) (setq p_fill_L p_bp_center))
                              (if (null p_fill_R) (setq p_fill_R p_bp_center))

                              (setq half_bp (max 250.0 (+ (max (distance p_bp_center p_fill_L) (distance p_bp_center p_fill_R)) 50.0)))

                              (setq p_bp_top_L (polar p_bp_center wl_dir half_bp))
                              (setq p_bp_top_R (polar p_bp_center (+ wl_dir pi) half_bp))

                              (setq p_ep_bot_L (polar p_ep_center (+ stiff-dir (/ pi 2.0)) stiff-half-w))
                              (setq p_ep_bot_R (polar p_ep_center (- stiff-dir (/ pi 2.0)) stiff-half-w))
                              (setq p_ep_top_L p_out_L)
                              (setq p_ep_top_R p_out_R)

                              (setq wedge-layer "_화타쐐기(Wedge)")
                              (setq wedge-color 2)

                              (draw-trimmed-line p_bp_top_L p_bp_top_R wedge-layer wedge-color)
                              (draw-trimmed-line p_fill_L p_ep_bot_L wedge-layer wedge-color)
                              (draw-trimmed-line p_fill_R p_ep_bot_R wedge-layer wedge-color)
                              (draw-trimmed-line p_ep_bot_L p_ep_bot_R wedge-layer wedge-color)
                              (draw-trimmed-line p_ep_bot_L p_ep_top_L wedge-layer wedge-color)
                              (draw-trimmed-line p_ep_bot_R p_ep_top_R wedge-layer wedge-color)
                              (draw-trimmed-line p_ep_top_L p_ep_top_R wedge-layer wedge-color)
                            )
                          )

                          (foreach line_def (list (list p_out_L 256 "BYLAYER") (list p_out_R 256 "BYLAYER") (list p_in_L 1 "DASHED") (list p_in_R 1 "DASHED"))
                            (setq p_start (car line_def) col (cadr line_def) ltype (caddr line_def) intervals '())

                            (setq adj_total_L (- total_L (tsp-project-dist p_start_center stiff-dir p_start)))

                            (foreach b_bounds brace_boundaries
                              (setq hits '())
                              (if (= (car b_bounds) "WEDGE")
                                (progn
                                  (setq ray_start (polar p_start stiff-dir -1e6))
                                  (setq ray_end (polar p_start stiff-dir 1e6))
                                  (foreach seg (cdr b_bounds)
                                    (setq p_x1 (inters ray_start ray_end (car seg) (cadr seg) T))
                                    (if p_x1
                                      (setq hits (append hits (list (tsp-project-dist p_start stiff-dir p_x1))))
                                      (progn
                                        (setq dx_col (- (car (car seg)) (car p_start)))
                                        (setq dy_col (- (cadr (car seg)) (cadr p_start)))
                                        (setq p_dist_col (abs (- (* dx_col (sin stiff-dir)) (* dy_col (cos stiff-dir)))))
                                        (if (< p_dist_col 0.1)
                                          (setq hits (append hits (list (tsp-project-dist p_start stiff-dir (car seg))
                                                                        (tsp-project-dist p_start stiff-dir (cadr seg)))))
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                              (if (>= (length hits) 1)
                                (progn
                                  (setq hits (vl-sort hits '<))
                                  (setq d1 (max 0.0 (car hits)))
                                  (setq d2 (max 0.0 (last hits)))
                                  (if (not (equal d1 d2 0.01))
                                    (setq intervals (append intervals (list (cons d1 d2))))
                                  )
                                )
                              )
                            )
                            (setq merged_intervals '())
                            (if intervals
                              (progn
                                (setq sorted (vl-sort intervals (function (lambda (a b) (< (car a) (car b))))))
                                (setq merged_intervals (list (car sorted)))
                                (foreach cur (cdr sorted)
                                  (setq last_m (last merged_intervals))
                                  (if (<= (car cur) (cdr last_m))
                                    (setq merged_intervals (subst (cons (car last_m) (max (cdr last_m) (cdr cur))) last_m merged_intervals))
                                    (setq merged_intervals (append merged_intervals (list cur)))
                                  )
                                )
                              )
                            )
                            (setq current_d 0.0)
                            (foreach cur_int merged_intervals
                              (if (> (car cur_int) current_d)
                                (progn
                                  (setq pt_from (polar p_start stiff-dir current_d) pt_to (polar p_start stiff-dir (car cur_int)))
                                  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 "_보강재(Brace)") (cons 62 col) (if (= ltype "DASHED") (cons 6 "DASHED") '(6 . "BYLAYER")) (if (= ltype "DASHED") (cons 48 (* (if (and (boundp '*tsp-scale-plan*) *tsp-scale-plan*) *tsp-scale-plan* 600.0) (/ 500.0 600.0) (if (= (getvar "MEASUREMENT") 1) (/ 1.0 25.0) 1.0))) '(48 . 1.0)) '(90 . 2) '(70 . 0) (list 10 (car pt_from) (cadr pt_from)) (list 10 (car pt_to) (cadr pt_to))))
                                )
                              )
                              (setq current_d (cdr cur_int))
                            )
                            (if (< current_d adj_total_L)
                              (progn
                                (setq pt_from (polar p_start stiff-dir current_d) pt_to (polar p_start stiff-dir adj_total_L))
                                (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 "_보강재(Brace)") (cons 62 col) (if (= ltype "DASHED") (cons 6 "DASHED") '(6 . "BYLAYER")) (if (= ltype "DASHED") (cons 48 (* (if (and (boundp '*tsp-scale-plan*) *tsp-scale-plan*) *tsp-scale-plan* 600.0) (/ 500.0 600.0) (if (= (getvar "MEASUREMENT") 1) (/ 1.0 25.0) 1.0))) '(48 . 1.0)) '(90 . 2) '(70 . 0) (list 10 (car pt_from) (cadr pt_from)) (list 10 (car pt_to) (cadr pt_to))))
                              )
                            )
                          )

                          (setq p_end_center (polar p_start_center stiff-dir total_L))
                          (setq p_end_L (polar p_end_center (+ stiff-dir (/ pi 2.0)) stiff-half-w))
                          (setq p_end_R (polar p_end_center (- stiff-dir (/ pi 2.0)) stiff-half-w))
                          (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 "_보강재(Brace)") (cons 62 256) '(90 . 2) '(70 . 0) (list 10 (car p_end_L) (cadr p_end_L)) (list 10 (car p_end_R) (cadr p_end_R))))
                        )
                      )
                    )
                  )
                  (setq stiff-idx (1+ stiff-idx))
                )
              )
            )
          )
        )
      )
    )
    (setq idx (1+ idx))
  )
  (princ)
)
