;;; ==========================================================================
;;; 파일명: TSP_5O_DCL_and_Strut.lsp
;;; 구  분: DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part O)
;;; 역  할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-draw-manual-strut]
;;; 포함 함수: tsp-draw-manual-strut
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-draw-manual-strut : 수동 버팀보 평면 객체 작도 (N등분/2H/말뚝/비평행띠장/경사버팀보 통합)
;;;;==========================================================================
(defun tsp-draw-manual-strut
  (pt1 pt2 sec-str jack-type is-double s-brace-opt seg-id
   / boundary-handle start-ent clean-sec-str strut-vals
     b tw h tf half-b half-tw strut-ang pt-dir jack-len plate-thick
     strut-offsets s-off sub-idx pt-jack-end pt-beam-start pt-beam-end
     pl1-p1 pl1-p2 pl1-p3 pl1-p4 pl2-p1 pl2-p2 pl2-p3 pl2-p4
     p1-out p2-out p3-out p4-out p1-in p2-in p3-in p4-in
     pt-jack-ins jack-block-name jack-rot strut-xdata strut-len
     total-L L-max num-div interval offsets spatial-idx is-valid-post
     invert-val p-center post-rot y_scale p-ins post-block new-ents nxt
     cur-pt1 cur-pt2 b-width stiff-enable slender-limit is-manual
     manual-interval post-enable post-layout post-invert post-side
     i strut-sys-half-width margin brace-len p-brace-1 p-brace-2
     post-half-h lateral-dist mid-gap-xdata corbel-enable corbel-conn
     corbel-params strut_left_p1 strut_left_p2
     strut_right_p1 strut_right_p2 dash-scale c_param c_p1 c_wnorm
     c_wdir c_cdir p1_f p2_f w_res corbel-base-pt ep1 ep2 pl_p1 pl_p2 pl_p3 pl_p4
     cb_start_1 cb_start_2 bound_p1 bound_p2 cut1 cut2 c_center w1_s w2_s
     w1_c w2_c tie-dist gap_centers gc gb_p1 gb_p2 gb_p3 gb_p4 gw1_s gw1_e gw2_s gw2_e
     is-corbel-direct jack-at-pt2 jack-start-1h post-pos-1h post-pp post-ys-1h corbel-dist
     corbel-xdata del-corbels c-key wedge-mark e-w ed-w gap-items gp-item gp-key gp-k1 gp-k2
     w1-info w2-info w1-dir w1-norm w2-dir w2-norm end1-angled end2-angled
     raw-pt1 raw-pt2 L1-1 L1-2 L2-1 L2-2 pA1 pA2 pB1 pB2
     jack-at-end1 jack-at-end2 is-conn-direct conn-1 conn-2
     conn_pts_A conn_pts_B brace_A1 brace_A2 brace_B1 brace_B2
     brace_A1_in brace_A2_in brace_B1_in brace_B2_in is-direct-1 is-direct-2
     tmp-p web1-p1 web1-p2 web2-p1 web2-p2
     w1-dir-L w1-dir-R w2-dir-L w2-dir-R d-w1-edge d-w2-edge c-dist-1 c-dist-2
     cdir-1L cdir-1R cdir-2L cdir-2R tie-dist-1 tie-dist-2
     base-ref-pt ref-len u1-proj min-clear k-grid u-grid off-local)

  ;; 작도 시작 전 마지막 객체를 기억
  (setq start-ent (entlast))
  (setq boundary-handle (tsp-get-entity-handle *tsp-boundary-ent*))

  ;; 1. 제원 파싱 및 레이어 생성 (2H 텍스트 치환 후 순수 규격 파싱)
  (setq clean-sec-str sec-str)
  (if (wcmatch clean-sec-str "2H *")
    (setq clean-sec-str (vl-string-subst "H " "2H " clean-sec-str))
  )

  (setq strut-vals (parse-h-spec clean-sec-str))
  (if (null strut-vals)
    (setq strut-vals '(300 300 10 15))
  )

  (setq h (nth 0 strut-vals)
        b (nth 1 strut-vals)
        tw (nth 2 strut-vals)
        tf (nth 3 strut-vals))

  (setq half-b (/ b 2.0)
        half-tw (/ tw 2.0))

  (create-layer-if-not-exists "_버팀보(Strut)" "3")
  (create-layer-if-not-exists "_중간말뚝(Post pile)" "3")
  (create-layer-if-not-exists "_보강재(Brace)" "7")
  (create-layer-if-not-exists "_화타쐐기(Wedge)" "2")
  (create-layer-if-not-exists "_철판(Plate)" "2")

  (if (not (tblsearch "LTYPE" "DASHED"))
    (vl-catch-all-apply
      'vla-load
      (list
        (vla-get-linetypes (vla-get-activedocument (vlax-get-acad-object)))
        "DASHED"
        (if (= (getvar "MEASUREMENT") 1) "acadiso.lin" "acad.lin")
      )
    )
  )

  (create-strut-jack-block)
  (create-strut-screw-jack-block)

  ;; 2. 각도, 방향 및 양단 띠장(Wale) 기하 정보 산출
  (setq strut-ang (angle pt1 pt2))
  (setq pt-dir (+ strut-ang (/ pi 2.0)))
  (setq strut-len (distance pt1 pt2))

  ;; 연속 보강재에서 각 개별 버팀보의 실제 길이를 즉시 참조할 수 있도록 캐시 등록
  (if (not (boundp '*tsp-strut-len-map*)) (setq *tsp-strut-len-map* '()))
  (setq *tsp-strut-len-map* (cons (cons (list (car pt1) (cadr pt1) 0.0) strut-len) *tsp-strut-len-map*))

  ;; 양 끝단(pt1, pt2)이 접합되는 실제 띠장의 방향(w-dir)과 내측 법선(w-norm) 검출
  (setq w1-info (tsp-get-wale-info-near-pt pt1 strut-ang (tsp-wale-lines-geo *tsp-boundary-ent*)))
  (setq w2-info (tsp-get-wale-info-near-pt pt2 (+ strut-ang pi) (tsp-wale-lines-geo *tsp-boundary-ent*)))
  (setq w1-dir (nth 2 w1-info) w1-norm (nth 3 w1-info))
  (setq w2-dir (nth 2 w2-info) w2-norm (nth 3 w2-info))

  ;; 각 단부가 해당 띠장과 직교(90°)하지 않는지(경사 접합 여부) 판별 (허용오차 약 0.45도)
  (setq end1-angled (> (abs (sin (- w1-norm strut-ang))) 0.008))
  (setq end2-angled (> (abs (sin (- w2-norm (+ strut-ang pi)))) 0.008))

  ;; 까치발 생성 옵션 및 연결 방식 파싱 (s-brace-opt의 9, 10번째 인덱스)
  (setq corbel-enable "0")
  (setq corbel-conn "")
  (if (and s-brace-opt (> (length s-brace-opt) 9))
    (setq corbel-enable (nth 9 s-brace-opt))
  )
  (if (and s-brace-opt (> (length s-brace-opt) 10))
    (setq corbel-conn (nth 10 s-brace-opt))
  )

  ;; [까치발 거리] 버팀보 중심선 ~ 까치발 기준점 거리(mm) : 사용자 입력(m) x 1000, 미입력 시 2H=1500 / 1H=1000
  (setq corbel-dist (* (atof (tsp-brace-opt-get s-brace-opt 13 "0")) 1000.0))
  (if (<= corbel-dist 0.0) (setq corbel-dist (if is-double 1500.0 1000.0)))
  (if (and (= corbel-enable "1") (<= corbel-dist (+ (if is-double (* b 1.5) (/ b 2.0)) (/ b 2.0))))
    (progn
      (princ (strcat "\n[경고] 까치발 거리(" (rtos (/ corbel-dist 1000.0) 2 3) "m)가 버팀보 폭에 비해 너무 작아 기본값으로 작도합니다."))
      (setq corbel-dist (if is-double 1500.0 1000.0))
    )
  )

  ;; 작도 시작
  (progn
    ;; 2H 맞버팀대 여부에 따른 작도 오프셋 (기본 0.0, 2H는 폭(b)만큼 이격)
    (setq b-width b)
    (setq strut-offsets (if is-double (list b-width (- b-width)) (list 0.0)))

    ;; 전체 버팀보 시스템의 중심 기준 절반 폭
    (setq strut-sys-half-width (if is-double (* b 1.5) (/ b 2.0)))

    ;; XDATA 공통 생성
    (if (not (tblsearch "APPID" "TSP_STRUT_SPEC"))
      (regapp "TSP_STRUT_SPEC")
    )

    (setq strut-xdata
      (list
        -3
        (list
          "TSP_STRUT_SPEC"
          (cons 1070 seg-id)
          (cons 1040 (float (* strut-sys-half-width 2.0)))
          (cons 1010 pt1)
          (cons 1010 pt2)
          (cons 1040 (float h))
          (cons 1040 (float tw))
          (cons 1040 (float tf))
          (cons 1000 "MANUAL_STRUT")
          (cons 1000 boundary-handle)
          (cons 1040 (float b))
        )
      )
    )

    ;; 3. 메인 부재(Flange, Web), 단부 이음부(화타쐐기/마감철판) 및 잭(Jack) 반복 작도
    (setq sub-idx 0)

    ;; [1H 확장] 1H 잭 평면 위치 : 사용자 지정 시작 방향(위/아래) + 버팀보 순서(직교 C.T.C 기준)대로 위/아래 교차
    (setq jack-at-pt2 nil)
    (if (not is-double)
      (progn
        (setq jack-start-1h (tsp-brace-opt-get s-brace-opt 11 "0"))
        (setq jack-at-pt2
          (tsp-1h-jack-at-pt2 pt1 pt2
            (tsp-1h-jack-want-top
              (if (and (boundp '*tsp-1h-jack-base*) *tsp-1h-jack-base*) *tsp-1h-jack-base* pt1)
              pt1 pt-dir
              (if (and (boundp '*tsp-1h-jack-ctc*) (numberp *tsp-1h-jack-ctc*)) *tsp-1h-jack-ctc* 5000.0)
              jack-start-1h)))
      )
    )

    (if (or (equal jack-type "스크류잭") (equal jack-type "1"))
      (setq jack-block-name "스크류잭")
      (setq jack-block-name "유압잭")
    )

    (foreach s-off strut-offsets
      (setq raw-pt1 (polar pt1 pt-dir s-off))
      (setq raw-pt2 (polar pt2 pt-dir s-off))

      ;; 2H 맞버팀대 등 각 개별 H형강 중심선이 양단 띠장 내측선과 만나는 실제 교차점 산출
      (setq cur-pt1 (inters raw-pt1 raw-pt2 pt1 (polar pt1 w1-dir 1000.0) nil))
      (if (null cur-pt1) (setq cur-pt1 raw-pt1))
      (setq cur-pt2 (inters raw-pt1 raw-pt2 pt2 (polar pt2 w2-dir 1000.0) nil))
      (if (null cur-pt2) (setq cur-pt2 raw-pt2))

      (setq jack-len 583.0
            plate-thick 14.0)

      (setq jack-at-end2 (or (and is-double (= sub-idx 1)) (and (not is-double) jack-at-pt2)))
      (setq jack-at-end1 (not jack-at-end2))

      (if (and (not end1-angled) (not end2-angled))
        ;; [A] 양단 모두 띠장과 90° 직교하는 표준 구간 : 기존 직교 작도 로직 유지
        (progn
          (if jack-at-end2
            (progn
              (setq pt-jack-end (polar cur-pt2 (+ strut-ang pi) jack-len))
              (setq pt-beam-start (polar cur-pt1 strut-ang plate-thick))
              (setq pt-beam-end (polar pt-jack-end (+ strut-ang pi) plate-thick))
              (setq pt-jack-ins cur-pt2)
              (setq jack-rot (+ strut-ang pi))

              (setq pl1-p1 (polar cur-pt1 pt-dir half-b)
                    pl1-p2 (polar pt-beam-start pt-dir half-b))
              (setq pl1-p3 (polar pt-beam-start (- pt-dir pi) half-b)
                    pl1-p4 (polar cur-pt1 (- pt-dir pi) half-b))

              (setq pl2-p1 (polar pt-beam-end pt-dir half-b)
                    pl2-p2 (polar pt-jack-end pt-dir half-b))
              (setq pl2-p3 (polar pt-jack-end (- pt-dir pi) half-b)
                    pl2-p4 (polar pt-beam-end (- pt-dir pi) half-b))
            )
            (progn
              (setq pt-jack-end (polar cur-pt1 strut-ang jack-len))
              (setq pt-beam-start (polar pt-jack-end strut-ang plate-thick))
              (setq pt-beam-end (polar cur-pt2 (+ strut-ang pi) plate-thick))
              (setq pt-jack-ins cur-pt1)
              (setq jack-rot strut-ang)

              (setq pl1-p1 (polar pt-jack-end pt-dir half-b)
                    pl1-p2 (polar pt-beam-start pt-dir half-b))
              (setq pl1-p3 (polar pt-beam-start (- pt-dir pi) half-b)
                    pl1-p4 (polar pt-jack-end (- pt-dir pi) half-b))

              (setq pl2-p1 (polar pt-beam-end pt-dir half-b)
                    pl2-p2 (polar cur-pt2 pt-dir half-b))
              (setq pl2-p3 (polar cur-pt2 (- pt-dir pi) half-b)
                    pl2-p4 (polar pt-beam-end (- pt-dir pi) half-b))
            )
          )

          (setq p1-out (polar pt-beam-start pt-dir half-b)
                p2-out (polar pt-beam-end pt-dir half-b))
          (setq p3-out (polar pt-beam-start (- pt-dir pi) half-b)
                p4-out (polar pt-beam-end (- pt-dir pi) half-b))

          (entmake
            (append
              (list
                '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                '(8 . "_버팀보(Strut)") '(62 . 3) '(90 . 4) '(70 . 1)
                (list 10 (car p1-out) (cadr p1-out)) (list 10 (car p2-out) (cadr p2-out))
                (list 10 (car p4-out) (cadr p4-out)) (list 10 (car p3-out) (cadr p3-out)))
              (list strut-xdata)
            )
          )

          (entmake
            (append
              (list
                '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                '(8 . "_버팀보(Strut)") '(62 . 3) '(90 . 4) '(70 . 1)
                (list 10 (car pl1-p1) (cadr pl1-p1)) (list 10 (car pl1-p2) (cadr pl1-p2))
                (list 10 (car pl1-p3) (cadr pl1-p3)) (list 10 (car pl1-p4) (cadr pl1-p4)))
              (list strut-xdata)
            )
          )

          (entmake
            (append
              (list
                '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                '(8 . "_버팀보(Strut)") '(62 . 3) '(90 . 4) '(70 . 1)
                (list 10 (car pl2-p1) (cadr pl2-p1)) (list 10 (car pl2-p2) (cadr pl2-p2))
                (list 10 (car pl2-p3) (cadr pl2-p3)) (list 10 (car pl2-p4) (cadr pl2-p4)))
              (list strut-xdata)
            )
          )

          (setq p1-in (polar pt-beam-start pt-dir half-tw)
                p2-in (polar pt-beam-end pt-dir half-tw))
          (setq p3-in (polar pt-beam-start (- pt-dir pi) half-tw)
                p4-in (polar pt-beam-end (- pt-dir pi) half-tw))

          (entmake
            (append
              (list
                '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                '(8 . "_버팀보(Strut)") '(62 . 1) '(6 . "DASHED")
                (cons 48 (* (if (and (boundp '*tsp-scale-plan*) *tsp-scale-plan*) *tsp-scale-plan* 600.0) (/ 500.0 600.0) (if (= (getvar "MEASUREMENT") 1) (/ 1.0 25.0) 1.0)))
                '(90 . 2) '(70 . 0)
                (list 10 (car p1-in) (cadr p1-in)) (list 10 (car p2-in) (cadr p2-in)))
              (list strut-xdata)
            )
          )

          (entmake
            (append
              (list
                '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                '(8 . "_버팀보(Strut)") '(62 . 1) '(6 . "DASHED")
                (cons 48 (* (if (and (boundp '*tsp-scale-plan*) *tsp-scale-plan*) *tsp-scale-plan* 600.0) (/ 500.0 600.0) (if (= (getvar "MEASUREMENT") 1) (/ 1.0 25.0) 1.0)))
                '(90 . 2) '(70 . 0)
                (list 10 (car p3-in) (cadr p3-in)) (list 10 (car p4-in) (cadr p4-in)))
              (list strut-xdata)
            )
          )

          (entmake
            (list
              '(0 . "INSERT")
              (cons 2 jack-block-name)
              (cons 8 (if (equal jack-block-name "스크류잭") "_스크류잭" "_유압잭"))
              '(62 . 2)
              (cons 10 pt-jack-ins)
              '(41 . 1.0) '(42 . 1.0) '(43 . 1.0)
              (cons 50 jack-rot))
          )
        )
        ;; [B] 비평행 맞은편 띠장 또는 경사 버팀보 구간 : 까치발 연결 방식(s_conn)과 연동하여 화타쐐기/마감철판 접합 작도
        (progn
          (setq L1-1 (polar cur-pt1 pt-dir half-b) L1-2 (polar cur-pt2 pt-dir half-b))
          (setq L2-1 (polar cur-pt1 (- pt-dir pi) half-b) L2-2 (polar cur-pt2 (- pt-dir pi) half-b))

          (setq pA1 (inters cur-pt1 (polar cur-pt1 w1-dir 1000.0) L1-1 L1-2 nil))
          (setq pA2 (inters cur-pt1 (polar cur-pt1 w1-dir 1000.0) L2-1 L2-2 nil))
          (setq pB1 (inters cur-pt2 (polar cur-pt2 w2-dir 1000.0) L1-1 L1-2 nil))
          (setq pB2 (inters cur-pt2 (polar cur-pt2 w2-dir 1000.0) L2-1 L2-2 nil))
          (if (null pA1) (setq pA1 L1-1))
          (if (null pA2) (setq pA2 L2-1))
          (if (null pB1) (setq pB1 L1-2))
          (if (null pB2) (setq pB2 L2-2))

          (setq is-conn-direct (or (null corbel-conn) (= corbel-conn "") (= corbel-conn "마감철판 직결")))

          ;; --- 시작단(pt1) 처리 ---
          (if end1-angled
            (progn
              ;; 잭이 설치되는 경사 단부는 직결 선택 시에도 잭 지지를 위해 "화타쐐기 없음" 적용 (사보강재 동일 기준)
              (setq conn-1 (if (and is-conn-direct jack-at-end1) "화타쐐기 없음" corbel-conn))
              (setq conn_pts_A (tsp-draw-wedge-connection cur-pt1 pA1 pA2 strut-ang w1-dir w1-norm conn-1))
              (setq brace_A1 (car conn_pts_A) brace_A2 (cadr conn_pts_A))
              ;; 플랜지 좌/우 정렬 (+pt-dir 쪽이 brace_A1이 되도록 정렬하여 꼬임 방지)
              (if (< (tsp-project-dist cur-pt1 pt-dir brace_A1) 0.0)
                (setq tmp-p brace_A1 brace_A1 brace_A2 brace_A2 tmp-p)
              )
              (if jack-at-end1
                (progn
                  (setq pt-jack-ins (list (/ (+ (car brace_A1) (car brace_A2)) 2.0) (/ (+ (cadr brace_A1) (cadr brace_A2)) 2.0) 0.0))
                  (setq jack-rot strut-ang)
                  (setq brace_A1 (polar brace_A1 strut-ang jack-len)
                        brace_A2 (polar brace_A2 strut-ang jack-len))
                  (setq brace_A1_in (polar brace_A1 strut-ang plate-thick)
                        brace_A2_in (polar brace_A2 strut-ang plate-thick))
                  (setq is-direct-1 nil)
                )
                (progn
                  (setq is-direct-1 is-conn-direct)
                  (setq brace_A1_in (polar brace_A1 strut-ang (if is-direct-1 0.0 plate-thick))
                        brace_A2_in (polar brace_A2 strut-ang (if is-direct-1 0.0 plate-thick)))
                )
              )
              (if (not is-direct-1)
                (entmake
                  (append
                    (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                          '(8 . "_버팀보(Strut)") '(62 . 3) '(90 . 4) '(70 . 1)
                          (list 10 (car brace_A1) (cadr brace_A1)) (list 10 (car brace_A2) (cadr brace_A2))
                          (list 10 (car brace_A2_in) (cadr brace_A2_in)) (list 10 (car brace_A1_in) (cadr brace_A1_in)))
                    (list strut-xdata)))
              )
            )
            (progn
              ;; 시작단(pt1)이 띠장과 90° 직교하는 경우 (기본값 90° 버팀보의 작업 세그먼트 측)
              (setq is-direct-1 nil)
              (if jack-at-end1
                (progn
                  (setq pt-jack-ins cur-pt1 jack-rot strut-ang)
                  (setq pt-jack-end (polar cur-pt1 strut-ang jack-len))
                  (setq pt-beam-start (polar pt-jack-end strut-ang plate-thick))
                  (setq brace_A1 (polar pt-jack-end pt-dir half-b)
                        brace_A2 (polar pt-jack-end (- pt-dir pi) half-b))
                )
                (progn
                  (setq pt-beam-start (polar cur-pt1 strut-ang plate-thick))
                  (setq brace_A1 (polar cur-pt1 pt-dir half-b)
                        brace_A2 (polar cur-pt1 (- pt-dir pi) half-b))
                )
              )
              (setq brace_A1_in (polar pt-beam-start pt-dir half-b)
                    brace_A2_in (polar pt-beam-start (- pt-dir pi) half-b))
              (entmake
                (append
                  (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                        '(8 . "_버팀보(Strut)") '(62 . 3) '(90 . 4) '(70 . 1)
                        (list 10 (car brace_A1) (cadr brace_A1)) (list 10 (car brace_A2) (cadr brace_A2))
                        (list 10 (car brace_A2_in) (cadr brace_A2_in)) (list 10 (car brace_A1_in) (cadr brace_A1_in)))
                  (list strut-xdata)))
            )
          )

          ;; --- 끝단(pt2, 맞은편 띠장) 처리 ---
          (if end2-angled
            (progn
              (setq conn-2 (if (and is-conn-direct jack-at-end2) "화타쐐기 없음" corbel-conn))
              (setq conn_pts_B (tsp-draw-wedge-connection cur-pt2 pB1 pB2 (+ strut-ang pi) w2-dir w2-norm conn-2))
              (setq brace_B1 (car conn_pts_B) brace_B2 (cadr conn_pts_B))
              (if (< (tsp-project-dist cur-pt2 pt-dir brace_B1) 0.0)
                (setq tmp-p brace_B1 brace_B1 brace_B2 brace_B2 tmp-p)
              )
              (if jack-at-end2
                (progn
                  (setq pt-jack-ins (list (/ (+ (car brace_B1) (car brace_B2)) 2.0) (/ (+ (cadr brace_B1) (cadr brace_B2)) 2.0) 0.0))
                  (setq jack-rot (+ strut-ang pi))
                  (setq brace_B1 (polar brace_B1 (+ strut-ang pi) jack-len)
                        brace_B2 (polar brace_B2 (+ strut-ang pi) jack-len))
                  (setq brace_B1_in (polar brace_B1 (+ strut-ang pi) plate-thick)
                        brace_B2_in (polar brace_B2 (+ strut-ang pi) plate-thick))
                  (setq is-direct-2 nil)
                )
                (progn
                  (setq is-direct-2 is-conn-direct)
                  (setq brace_B1_in (polar brace_B1 (+ strut-ang pi) (if is-direct-2 0.0 plate-thick))
                        brace_B2_in (polar brace_B2 (+ strut-ang pi) (if is-direct-2 0.0 plate-thick)))
                )
              )
              (if (not is-direct-2)
                (entmake
                  (append
                    (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                          '(8 . "_버팀보(Strut)") '(62 . 3) '(90 . 4) '(70 . 1)
                          (list 10 (car brace_B1) (cadr brace_B1)) (list 10 (car brace_B2) (cadr brace_B2))
                          (list 10 (car brace_B2_in) (cadr brace_B2_in)) (list 10 (car brace_B1_in) (cadr brace_B1_in)))
                    (list strut-xdata)))
              )
            )
            (progn
              (setq is-direct-2 nil)
              (if jack-at-end2
                (progn
                  (setq pt-jack-ins cur-pt2 jack-rot (+ strut-ang pi))
                  (setq pt-jack-end (polar cur-pt2 (+ strut-ang pi) jack-len))
                  (setq pt-beam-end (polar pt-jack-end (+ strut-ang pi) plate-thick))
                  (setq brace_B1 (polar pt-jack-end pt-dir half-b)
                        brace_B2 (polar pt-jack-end (- pt-dir pi) half-b))
                )
                (progn
                  (setq pt-beam-end (polar cur-pt2 (+ strut-ang pi) plate-thick))
                  (setq brace_B1 (polar cur-pt2 pt-dir half-b)
                        brace_B2 (polar cur-pt2 (- pt-dir pi) half-b))
                )
              )
              (setq brace_B1_in (polar pt-beam-end pt-dir half-b)
                    brace_B2_in (polar pt-beam-end (- pt-dir pi) half-b))
              (entmake
                (append
                  (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                        '(8 . "_버팀보(Strut)") '(62 . 3) '(90 . 4) '(70 . 1)
                        (list 10 (car brace_B1) (cadr brace_B1)) (list 10 (car brace_B2) (cadr brace_B2))
                        (list 10 (car brace_B2_in) (cadr brace_B2_in)) (list 10 (car brace_B1_in) (cadr brace_B1_in)))
                  (list strut-xdata)))
            )
          )

          ;; H형강 외곽 플랜지 본체 작도
          (entmake
            (append
              (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                    '(8 . "_버팀보(Strut)") '(62 . 3) '(90 . 4) '(70 . 1)
                    (list 10 (car brace_A1_in) (cadr brace_A1_in))
                    (list 10 (car brace_B1_in) (cadr brace_B1_in))
                    (list 10 (car brace_B2_in) (cadr brace_B2_in))
                    (list 10 (car brace_A2_in) (cadr brace_A2_in)))
              (list strut-xdata)))

          ;; H형강 웹(Web) 파선 작도 (직결 단부 경사면까지 정밀 교차 연장)
          (setq web1-p1 (polar cur-pt1 pt-dir half-tw)
                web1-p2 (polar cur-pt2 pt-dir half-tw))
          (setq web2-p1 (polar cur-pt1 (- pt-dir pi) half-tw)
                web2-p2 (polar cur-pt2 (- pt-dir pi) half-tw))
          (setq p1-in (cond ((inters web1-p1 web1-p2 brace_A1_in brace_A2_in nil)) (web1-p1)))
          (setq p2-in (cond ((inters web1-p1 web1-p2 brace_B1_in brace_B2_in nil)) (web1-p2)))
          (setq p3-in (cond ((inters web2-p1 web2-p2 brace_A1_in brace_A2_in nil)) (web2-p1)))
          (setq p4-in (cond ((inters web2-p1 web2-p2 brace_B1_in brace_B2_in nil)) (web2-p2)))

          (entmake
            (append
              (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                    '(8 . "_버팀보(Strut)") '(62 . 1) '(6 . "DASHED")
                    (cons 48 (* (if (and (boundp '*tsp-scale-plan*) *tsp-scale-plan*) *tsp-scale-plan* 600.0) (/ 500.0 600.0) (if (= (getvar "MEASUREMENT") 1) (/ 1.0 25.0) 1.0)))
                    '(90 . 2) '(70 . 0)
                    (list 10 (car p1-in) (cadr p1-in)) (list 10 (car p2-in) (cadr p2-in)))
              (list strut-xdata)))

          (entmake
            (append
              (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                    '(8 . "_버팀보(Strut)") '(62 . 1) '(6 . "DASHED")
                    (cons 48 (* (if (and (boundp '*tsp-scale-plan*) *tsp-scale-plan*) *tsp-scale-plan* 600.0) (/ 500.0 600.0) (if (= (getvar "MEASUREMENT") 1) (/ 1.0 25.0) 1.0)))
                    '(90 . 2) '(70 . 0)
                    (list 10 (car p3-in) (cadr p3-in)) (list 10 (car p4-in) (cadr p4-in)))
              (list strut-xdata)))

          ;; 잭(Jack) 블록 삽입
          (entmake
            (list
              '(0 . "INSERT")
              (cons 2 jack-block-name)
              (cons 8 (if (equal jack-block-name "스크류잭") "_스크류잭" "_유압잭"))
              '(62 . 2)
              (cons 10 pt-jack-ins)
              '(41 . 1.0) '(42 . 1.0) '(43 . 1.0)
              (cons 50 jack-rot))
          )
        )
      )

      (setq sub-idx (1+ sub-idx))
    )

  ;; =========================================================
  ;; 까치발(Corbel) 실물 작도 (비평행/경사 띠장 진행방향 및 버팀보 각도 완벽 연동)
  ;; =========================================================
  (if (= corbel-enable "1")
    (progn
      ;; [까치발 레이어] 까치발 전용 레이어 생성 및 사용자가 삭제한 까치발 목록 로드
      (create-layer-if-not-exists "_까치발(Corbel)" "3")
      (if (not (tblsearch "APPID" "TSP_CORBEL")) (regapp "TSP_CORBEL"))
      (setq del-corbels (vlax-ldata-get "TSP_DICT" "DELETED_CORBELS"))

      ;; 각 단부에서 버팀보의 좌측(+pt-dir) 및 우측(-pt-dir)으로 향하는 실제 띠장 진행 방향 산출
      (if (> (sin (- w1-dir strut-ang)) 0.0)
        (setq w1-dir-L w1-dir w1-dir-R (+ w1-dir pi))
        (setq w1-dir-L (+ w1-dir pi) w1-dir-R w1-dir)
      )
      (if (> (sin (- w2-dir strut-ang)) 0.0)
        (setq w2-dir-L w2-dir w2-dir-R (+ w2-dir pi))
        (setq w2-dir-L (+ w2-dir pi) w2-dir-R w2-dir)
      )

      ;; 실제 띠장 진행방향과 버팀보 진행방향의 이등분선으로 까치발 각도(c_cdir) 산출
      (setq cdir-1L (tsp-angle-bisector w1-dir-R strut-ang))
      (setq cdir-1R (tsp-angle-bisector w1-dir-L strut-ang))
      (setq cdir-2L (tsp-angle-bisector w2-dir-R (+ strut-ang pi)))
      (setq cdir-2R (tsp-angle-bisector w2-dir-L (+ strut-ang pi)))

      ;; 버팀보 양 끝단에서 좌/우 까치발이 버팀보 외곽선과 만나는 축방향 거리(tie-dist-1, tie-dist-2)를 먼저 결정하고,
      ;; 그 대칭 기준점에서 각 까치발 각도(cdir)의 역방향으로 띠장 내측선과 만나는 교점을 역산하여
      ;; 사선(빗각) 띠장에서도 좌/우 까치발이 버팀보의 정확히 같은 위치에 대칭으로 붙도록 보정
      (setq tie-dist (- corbel-dist strut-sys-half-width))
      (setq tie-dist-1
        (if end1-angled
          (+ (* strut-sys-half-width (/ (abs (cos (- w1-norm strut-ang))) (max 0.25 (abs (sin (- w1-dir-L strut-ang)))))) tie-dist)
          tie-dist
        )
      )
      (setq tie-dist-2
        (if end2-angled
          (+ (* strut-sys-half-width (/ (abs (cos (- w2-norm (+ strut-ang pi)))) (max 0.25 (abs (sin (- w2-dir-L strut-ang)))))) tie-dist)
          tie-dist
        )
      )

      ;; 버팀보 좌/우 외곽선 위의 대칭 목표점(target)에서 띠장 내측선으로 역투사하여 까치발 시작 기준점 산출
      (setq c-dist-1 corbel-dist c-dist-2 corbel-dist)
      (setq corbel-params (list
        (list
          (cond
            ((inters (polar (polar pt1 strut-ang tie-dist-1) (+ strut-ang (/ pi 2.0)) strut-sys-half-width)
                     (polar (polar (polar pt1 strut-ang tie-dist-1) (+ strut-ang (/ pi 2.0)) strut-sys-half-width) (+ cdir-1L pi) 1000.0)
                     pt1 (polar pt1 w1-dir-L 1000.0) nil))
            ((polar pt1 w1-dir-L corbel-dist))
          )
          w1-norm w1-dir-L cdir-1L (polar pt1 (+ strut-ang (/ pi 2.0)) 100.0)
        )
        (list
          (cond
            ((inters (polar (polar pt1 strut-ang tie-dist-1) (- strut-ang (/ pi 2.0)) strut-sys-half-width)
                     (polar (polar (polar pt1 strut-ang tie-dist-1) (- strut-ang (/ pi 2.0)) strut-sys-half-width) (+ cdir-1R pi) 1000.0)
                     pt1 (polar pt1 w1-dir-R 1000.0) nil))
            ((polar pt1 w1-dir-R corbel-dist))
          )
          w1-norm w1-dir-R cdir-1R (polar pt1 (- strut-ang (/ pi 2.0)) 100.0)
        )
        (list
          (cond
            ((inters (polar (polar pt2 (+ strut-ang pi) tie-dist-2) (+ strut-ang (/ pi 2.0)) strut-sys-half-width)
                     (polar (polar (polar pt2 (+ strut-ang pi) tie-dist-2) (+ strut-ang (/ pi 2.0)) strut-sys-half-width) (+ cdir-2L pi) 1000.0)
                     pt2 (polar pt2 w2-dir-L 1000.0) nil))
            ((polar pt2 w2-dir-L corbel-dist))
          )
          w2-norm w2-dir-L cdir-2L (polar pt2 (+ strut-ang (/ pi 2.0)) 100.0)
        )
        (list
          (cond
            ((inters (polar (polar pt2 (+ strut-ang pi) tie-dist-2) (- strut-ang (/ pi 2.0)) strut-sys-half-width)
                     (polar (polar (polar pt2 (+ strut-ang pi) tie-dist-2) (- strut-ang (/ pi 2.0)) strut-sys-half-width) (+ cdir-2R pi) 1000.0)
                     pt2 (polar pt2 w2-dir-R 1000.0) nil))
            ((polar pt2 w2-dir-R corbel-dist))
          )
          w2-norm w2-dir-R cdir-2R (polar pt2 (- strut-ang (/ pi 2.0)) 100.0)
        )
      ))

      ;; 까치발이 달라붙을 버팀보 외곽선 양측 경계 좌표
      (setq strut_left_p1 (polar pt1 (+ strut-ang (/ pi 2.0)) strut-sys-half-width))
      (setq strut_left_p2 (polar pt2 (+ strut-ang (/ pi 2.0)) strut-sys-half-width))
      (setq strut_right_p1 (polar pt1 (- strut-ang (/ pi 2.0)) strut-sys-half-width))
      (setq strut_right_p2 (polar pt2 (- strut-ang (/ pi 2.0)) strut-sys-half-width))

      (setq dash-scale (cons 48 (* (if (and (boundp '*tsp-scale-plan*) *tsp-scale-plan*) *tsp-scale-plan* 600.0) (/ 500.0 600.0) (if (= (getvar "MEASUREMENT") 1) (/ 1.0 25.0) 1.0))))

      (foreach c_param corbel-params
       (setq c-key (list (car (nth 4 c_param)) (cadr (nth 4 c_param)) 0.0)) ;; XDATA 1010용 3D 좌표
       ;; [까치발 삭제] 사용자가 삭제한 까치발은 작도하지 않음
       (if (not (tsp-corbel-deleted-p c-key del-corbels))
        (progn
        (setq c_p1 (nth 0 c_param) c_wnorm (nth 1 c_param) c_wdir (nth 2 c_param) c_cdir (nth 3 c_param))
        ;; 까치발 구성 객체 공통 XDATA (기존 버팀보 XDATA + 까치발 식별키)
        (setq corbel-xdata (append strut-xdata (list (list "TSP_CORBEL" (cons 1010 c-key)))))

        ;; 1) 연결부(화타쐐기) 함수 호출을 위한 까치발 축 직교 폭 산출
        (setq p1_f (polar c_p1 (+ c_cdir (/ pi 2.0)) half-b))
        (setq p2_f (polar c_p1 (- c_cdir (/ pi 2.0)) half-b))

        ;; 화타쐐기 함수가 반환하는 정확한 끝점(w_res)을 변수로 회수
        (setq wedge-mark (entlast))
        (setq w_res (tsp-draw-wedge-connection c_p1 p1_f p2_f c_cdir c_wdir c_wnorm corbel-conn))
        (setq ep1 (car w_res) ep2 (cadr w_res))
        ;; 까치발에 딸린 화타쐐기 객체도 까치발 레이어로 이동 + 식별키 부착
        (tsp-corbel-adopt-ents wedge-mark c-key)

        ;; [마감철판 직결] t14 연결재 없이 까치발 몸통이 마감철판에 바로 직결
        (setq is-corbel-direct (or (null corbel-conn) (= corbel-conn "") (= corbel-conn "마감철판 직결")))

        ;; 2) t14 마감 철판 작도 (직결 시 오프셋 0, 작도 생략)
        (setq pl_p1 ep1)
        (setq pl_p2 ep2)
        (setq pl_p3 (polar ep2 c_cdir (if is-corbel-direct 0.0 14.0)))
        (setq pl_p4 (polar ep1 c_cdir (if is-corbel-direct 0.0 14.0)))

        (if (not is-corbel-direct)
          (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_까치발(Corbel)") '(62 . 3) '(90 . 4) '(70 . 1)
                                 (list 10 (car pl_p1) (cadr pl_p1)) (list 10 (car pl_p2) (cadr pl_p2))
                                 (list 10 (car pl_p3) (cadr pl_p3)) (list 10 (car pl_p4) (cadr pl_p4)))
                           (list corbel-xdata)))
        )

        ;; 까치발 H형강 몸통 시작점(철판 끝) 업데이트
        (setq cb_start_1 pl_p4)
        (setq cb_start_2 pl_p3)

        ;; 3) 현재 그리는 까치발이 버팀보의 좌/우 중 어느 경계선에 붙는지 판별
        (if (> (sin (- c_wdir strut-ang)) 0.0)
          (progn (setq bound_p1 strut_left_p1 bound_p2 strut_left_p2))
          (progn (setq bound_p1 strut_right_p1 bound_p2 strut_right_p2))
        )

        ;; 4) 까치발 시작점부터 버팀보 외곽선까지 가상 교차(Trim) 계산
        (setq cut1 (inters cb_start_1 (polar cb_start_1 c_cdir 1000.0) bound_p1 bound_p2 nil))
        (setq cut2 (inters cb_start_2 (polar cb_start_2 c_cdir 1000.0) bound_p1 bound_p2 nil))

        (if (and cut1 cut2)
          (progn
            ;; 까치발 외곽 플랜지 선을 하나의 '폐합된 폴리선'으로 작도
            (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_까치발(Corbel)") '(62 . 3) '(90 . 4) '(70 . 1)
                                   (list 10 (car cb_start_1) (cadr cb_start_1)) (list 10 (car cut1) (cadr cut1))
                                   (list 10 (car cut2) (cadr cut2)) (list 10 (car cb_start_2) (cadr cb_start_2)))
                             (list corbel-xdata)))

            ;; 까치발 내측 웹(Web) 점선 작도
            (setq c_center (polar cb_start_1 (angle cb_start_1 cb_start_2) (/ (distance cb_start_1 cb_start_2) 2.0)))
            (setq w1_s (polar c_center (+ c_cdir (/ pi 2.0)) half-tw))
            (setq w2_s (polar c_center (- c_cdir (/ pi 2.0)) half-tw))
            ;; [마감철판 직결] 웹 파선 시작점을 마감철판 윗면(사선 단부)까지 연장
            (if is-corbel-direct
              (progn
                (setq w1_s (cond ((inters w1_s (polar w1_s c_cdir 1000.0) cb_start_1 cb_start_2 nil)) (w1_s)))
                (setq w2_s (cond ((inters w2_s (polar w2_s c_cdir 1000.0) cb_start_1 cb_start_2 nil)) (w2_s)))
              )
            )
            (setq w1_c (inters w1_s (polar w1_s c_cdir 1000.0) bound_p1 bound_p2 nil))
            (setq w2_c (inters w2_s (polar w2_s c_cdir 1000.0) bound_p1 bound_p2 nil))

            (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_까치발(Corbel)") '(62 . 1) '(6 . "DASHED") dash-scale '(90 . 2) '(70 . 0) (list 10 (car w1_s) (cadr w1_s)) (list 10 (car w1_c) (cadr w1_c))) (list corbel-xdata)))
            (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_까치발(Corbel)") '(62 . 1) '(6 . "DASHED") dash-scale '(90 . 2) '(70 . 0) (list 10 (car w2_s) (cadr w2_s)) (list 10 (car w2_c) (cadr w2_c))) (list corbel-xdata)))
          )
        )
        )
       )
      )

      ;; 가운데 지지부(맞버팀대 간극 보강)는 2H에서만 작도, 1H는 생략 (위에서 산출한 좌/우 대칭 tie-dist-1, tie-dist-2와 일치시킴)
      ;; [까치발 삭제] 가운데 지지부 식별키 = 버팀보 끝점에서 버팀보 축 방향 100mm 점
      (setq gap-items (if is-double
                        (list (list (polar pt1 strut-ang tie-dist-1) (polar pt1 strut-ang 100.0) (nth 4 (nth 0 corbel-params)) (nth 4 (nth 1 corbel-params)))
                              (list (polar pt2 (+ strut-ang pi) tie-dist-2) (polar pt2 (+ strut-ang pi) 100.0) (nth 4 (nth 2 corbel-params)) (nth 4 (nth 3 corbel-params))))
                        '()))
      (setq gap_centers '())
      (foreach gp-item gap-items
        (setq gp-key (nth 1 gp-item) gp-k1 (nth 2 gp-item) gp-k2 (nth 3 gp-item))
        (if (not (or (tsp-corbel-deleted-p gp-key del-corbels)
                     (and (tsp-corbel-deleted-p gp-k1 del-corbels) (tsp-corbel-deleted-p gp-k2 del-corbels))))
          (setq gap_centers (append gap_centers (list (list (car gp-item) (list (car gp-key) (cadr gp-key) 0.0)))))
        )
      )

      (foreach gp-item gap_centers
        (setq gc (car gp-item))
        (setq corbel-xdata (append strut-xdata (list (list "TSP_CORBEL" (cons 1010 (cadr gp-item))))))

        ;; 외곽 플랜지 선 - 길이는 pt-dir 방향, 간격은 strut-ang 방향
        (setq fb1_s (polar (polar gc strut-ang half-b) pt-dir half-b))
        (setq fb1_e (polar (polar gc strut-ang half-b) (- pt-dir pi) half-b))
        (setq fb2_s (polar (polar gc (+ strut-ang pi) half-b) pt-dir half-b))
        (setq fb2_e (polar (polar gc (+ strut-ang pi) half-b) (- pt-dir pi) half-b))

        (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_까치발(Corbel)") '(62 . 3) '(90 . 2) '(70 . 0)
                               (list 10 (car fb1_s) (cadr fb1_s)) (list 10 (car fb1_e) (cadr fb1_e)))
                         (list corbel-xdata)))
        (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_까치발(Corbel)") '(62 . 3) '(90 . 2) '(70 . 0)
                               (list 10 (car fb2_s) (cadr fb2_s)) (list 10 (car fb2_e) (cadr fb2_e)))
                         (list corbel-xdata)))

        ;; 보강부 웹 (점선)
        (setq gw1_s (polar (polar gc strut-ang half-tw) pt-dir half-b))
        (setq gw1_e (polar (polar gc strut-ang half-tw) (- pt-dir pi) half-b))
        (setq gw2_s (polar (polar gc (+ strut-ang pi) half-tw) pt-dir half-b))
        (setq gw2_e (polar (polar gc (+ strut-ang pi) half-tw) (- pt-dir pi) half-b))

        (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_까치발(Corbel)") '(62 . 1) '(6 . "DASHED") dash-scale '(90 . 2) '(70 . 0) (list 10 (car gw1_s) (cadr gw1_s)) (list 10 (car gw1_e) (cadr gw1_e))) (list corbel-xdata)))
        (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_까치발(Corbel)") '(62 . 1) '(6 . "DASHED") dash-scale '(90 . 2) '(70 . 0) (list 10 (car gw2_s) (cadr gw2_s)) (list 10 (car gw2_e) (cadr gw2_e))) (list corbel-xdata)))
      )
    )
  )

  ;; 4. 보강재(Brace) 및 중간말뚝 배치 로직
  ;;    기울어진 버팀보 그룹에서도 보강재와 중간말뚝이 항상 버팀보에 수직(90°)인 일직선상에 놓이도록
  ;;    그룹 기준점(base-ref-pt) 및 기준길이(ref-len)의 버팀보 축(strut-ang) 투영 좌표를 기준으로 배치
  (if s-brace-opt
    (progn
      (setq stiff-enable   (nth 0 s-brace-opt)
            slender-limit (nth 1 s-brace-opt)
            is-manual     (nth 2 s-brace-opt)
            manual-interval (nth 3 s-brace-opt))

      (setq margin (atof (nth 4 s-brace-opt)))
      (setq post-enable (nth 5 s-brace-opt)
            post-layout (nth 6 s-brace-opt)
            post-invert (nth 7 s-brace-opt)
            post-side   (nth 8 s-brace-opt))

      (setq base-ref-pt (if (and (boundp '*tsp-1h-jack-base*) *tsp-1h-jack-base*) *tsp-1h-jack-base* pt1))
      (setq ref-len (if (and (boundp '*tsp-group-ref-len*) (numberp *tsp-group-ref-len*) (> *tsp-group-ref-len* 0.0)) *tsp-group-ref-len* strut-len))
      (setq u1-proj (tsp-project-dist base-ref-pt strut-ang pt1))

      (setq total-L (/ ref-len 1000.0))
      (setq L-max (tsp-calc-unsupported-length strut-vals (if (= slender-limit "0") 100.0 120.0)))

      (setq offsets '())
      (setq *tsp-last-strut-len* ref-len)

      (if (or (> total-L L-max) (= is-manual "1"))
        (progn
          (if (= is-manual "1")
            (setq interval (* (atof manual-interval) 1000.0))
            (progn
              (setq num-div (fix (+ (/ total-L L-max) 0.9999)))
              (if (< num-div 1) (setq num-div 1))
              (setq interval (/ ref-len num-div))
              (setq *tsp-last-calc-interval* (/ interval 1000.0))
            )
          )
          (if (> interval 10.0)
            (progn
              (setq min-clear (min 650.0 (* interval 0.15)))
              ;; 까치발 생성이 켜져 있으면 양 끝단 까치발 도달 거리(+ 부재폭 및 여유장) 안쪽에는 보강재와 중간말뚝이 생성되지 않도록 방어
              (setq min-clear-1
                (if (= corbel-enable "1")
                  (max min-clear (+ (if tie-dist-1 tie-dist-1 (- corbel-dist strut-sys-half-width)) b 200.0))
                  min-clear
                )
              )
              (setq min-clear-2
                (if (= corbel-enable "1")
                  (max min-clear (+ (if tie-dist-2 tie-dist-2 (- corbel-dist strut-sys-half-width)) b 200.0))
                  min-clear
                )
              )
              ;; 연속 보강재(tsp-draw-realtime-continuous-braces)에서도 동일 한계선을 참조할 수 있도록 캐시 등록
              (if (not (boundp '*tsp-strut-clear-map*)) (setq *tsp-strut-clear-map* '()))
              (setq *tsp-strut-clear-map*
                (cons (cons (list (car pt1) (cadr pt1) 0.0) (cons min-clear-1 min-clear-2)) *tsp-strut-clear-map*)
              )
              (setq k-grid -30)
              (while (<= k-grid 50)
                (setq u-grid (* k-grid interval))
                (setq off-local (- u-grid u1-proj))
                (if (and (> off-local min-clear-1) (< off-local (- strut-len min-clear-2)))
                  (setq offsets (append offsets (list off-local)))
                )
                (setq k-grid (1+ k-grid))
              )
            )
          )
        )
      )

      (if (> (length offsets) 0)
        (progn
          (setq spatial-idx 0)
          (setq post-block (create-post-pile-block h b tw tf))

          (setq brace-len (+ strut-sys-half-width margin))

          (foreach off offsets
            (setq p-center (polar pt1 strut-ang off))

            (if (= stiff-enable "1")
              (progn
                (setq p-brace-1 (polar p-center (+ strut-ang (/ pi 2.0)) brace-len))
                (setq p-brace-2 (polar p-center (- strut-ang (/ pi 2.0)) brace-len))

                (setq p-brace-1-inner (polar p-center (+ strut-ang (/ pi 2.0)) strut-sys-half-width))
                (setq p-brace-2-inner (polar p-center (- strut-ang (/ pi 2.0)) strut-sys-half-width))

                (setq f1-1 (polar p-brace-1 strut-ang half-b))
                (setq f1-2 (polar p-brace-1-inner strut-ang half-b))
                (setq f1-3 (polar p-brace-1-inner (+ strut-ang pi) half-b))
                (setq f1-4 (polar p-brace-1 (+ strut-ang pi) half-b))

                (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 256) '(90 . 2) '(70 . 0) (list 10 (car f1-1) (cadr f1-1)) (list 10 (car f1-4) (cadr f1-4))) (list strut-xdata)))
                (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 256) '(90 . 2) '(70 . 0) (list 10 (car f1-2) (cadr f1-2)) (list 10 (car f1-3) (cadr f1-3))) (list strut-xdata)))

                (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 256) '(90 . 2) '(70 . 0) (list 10 (car f1-1) (cadr f1-1)) (list 10 (car f1-2) (cadr f1-2))) (list strut-xdata)))
                (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 256) '(90 . 2) '(70 . 0) (list 10 (car f1-4) (cadr f1-4)) (list 10 (car f1-3) (cadr f1-3))) (list strut-xdata)))

                (setq f2-1 (polar p-brace-2-inner strut-ang half-b))
                (setq f2-2 (polar p-brace-2 strut-ang half-b))
                (setq f2-3 (polar p-brace-2 (+ strut-ang pi) half-b))
                (setq f2-4 (polar p-brace-2-inner (+ strut-ang pi) half-b))

                (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 256) '(90 . 2) '(70 . 0) (list 10 (car f2-1) (cadr f2-1)) (list 10 (car f2-4) (cadr f2-4))) (list strut-xdata)))
                (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 256) '(90 . 2) '(70 . 0) (list 10 (car f2-2) (cadr f2-2)) (list 10 (car f2-3) (cadr f2-3))) (list strut-xdata)))
                (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 256) '(90 . 2) '(70 . 0) (list 10 (car f2-1) (cadr f2-1)) (list 10 (car f2-2) (cadr f2-2))) (list strut-xdata)))
                (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 256) '(90 . 2) '(70 . 0) (list 10 (car f2-4) (cadr f2-4)) (list 10 (car f2-3) (cadr f2-3))) (list strut-xdata)))

                (setq w1-1 (polar p-brace-1 strut-ang half-tw))
                (setq w1-2 (polar p-brace-1-inner strut-ang half-tw))
                (setq w1-3 (polar p-brace-1 (+ strut-ang pi) half-tw))
                (setq w1-4 (polar p-brace-1-inner (+ strut-ang pi) half-tw))

                (entmake
                  (append
                    (list
                      '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                      '(8 . "_보강재(Brace)") '(62 . 1) '(6 . "DASHED")
                      (cons 48 (* (if (and (boundp '*tsp-scale-plan*) *tsp-scale-plan*) *tsp-scale-plan* 600.0) (/ 500.0 600.0)))
                      '(90 . 2) '(70 . 0)
                      (list 10 (car w1-1) (cadr w1-1)) (list 10 (car w1-2) (cadr w1-2)))
                    (list strut-xdata)
                  )
                )

                (entmake
                  (append
                    (list
                      '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                      '(8 . "_보강재(Brace)") '(62 . 1) '(6 . "DASHED")
                      (cons 48 (* (if (and (boundp '*tsp-scale-plan*) *tsp-scale-plan*) *tsp-scale-plan* 600.0) (/ 500.0 600.0)))
                      '(90 . 2) '(70 . 0)
                      (list 10 (car w1-3) (cadr w1-3)) (list 10 (car w1-4) (cadr w1-4)))
                    (list strut-xdata)
                  )
                )

                (setq w2-1 (polar p-brace-2-inner strut-ang half-tw))
                (setq w2-2 (polar p-brace-2 strut-ang half-tw))
                (setq w2-3 (polar p-brace-2-inner (+ strut-ang pi) half-tw))
                (setq w2-4 (polar p-brace-2 (+ strut-ang pi) half-tw))

                (entmake
                  (append
                    (list
                      '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                      '(8 . "_보강재(Brace)") '(62 . 1) '(6 . "DASHED")
                      (cons 48 (* (if (and (boundp '*tsp-scale-plan*) *tsp-scale-plan*) *tsp-scale-plan* 600.0) (/ 500.0 600.0)))
                      '(90 . 2) '(70 . 0)
                      (list 10 (car w2-1) (cadr w2-1)) (list 10 (car w2-2) (cadr w2-2)))
                    (list strut-xdata)
                  )
                )

                (entmake
                  (append
                    (list
                      '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                      '(8 . "_보강재(Brace)") '(62 . 1) '(6 . "DASHED")
                      (cons 48 (* (if (and (boundp '*tsp-scale-plan*) *tsp-scale-plan*) *tsp-scale-plan* 600.0) (/ 500.0 600.0)))
                      '(90 . 2) '(70 . 0)
                      (list 10 (car w2-3) (cadr w2-3)) (list 10 (car w2-4) (cadr w2-4)))
                    (list strut-xdata)
                  )
                )

                (if is-double
                  (progn
                    (if (not (tblsearch "APPID" "TSP_LISP_BRACE")) (regapp "TSP_LISP_BRACE"))
                    (setq mid-gap-xdata
                      (list -3
                        (list "TSP_LISP_BRACE"
                          (cons 1070 seg-id) (cons 1040 0.0) (cons 1010 '(0.0 0.0 0.0))
                          (cons 1010 '(0.0 0.0 0.0)) (cons 1040 (float h)) (cons 1040 (float tw))
                          (cons 1040 (float tf)) (cons 1000 "MID_GAP") (cons 1000 boundary-handle)
                        )
                      )
                    )

                    (setq p-mid-1 (polar p-center (+ strut-ang (/ pi 2.0)) half-b))
                    (setq p-mid-2 (polar p-center (- strut-ang (/ pi 2.0)) half-b))

                    (setq fm-1 (polar p-mid-1 strut-ang half-b))
                    (setq fm-2 (polar p-mid-2 strut-ang half-b))
                    (setq fm-3 (polar p-mid-2 (+ strut-ang pi) half-b))
                    (setq fm-4 (polar p-mid-1 (+ strut-ang pi) half-b))

                    (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 256) '(90 . 2) '(70 . 0) (list 10 (car fm-1) (cadr fm-1)) (list 10 (car fm-2) (cadr fm-2))) (list mid-gap-xdata)))
                    (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 256) '(90 . 2) '(70 . 0) (list 10 (car fm-4) (cadr fm-4)) (list 10 (car fm-3) (cadr fm-3))) (list mid-gap-xdata)))
                  )
                )
              )
            )

            (setq is-valid-post T)
            (if (and (= post-enable "1") (= post-layout "1"))
              (progn
                (setq invert-val (if (= post-invert "1") 1 0))
                (if (/= (rem (+ spatial-idx invert-val) 2) 0)
                  (setq is-valid-post nil)
                )
              )
            )

            (if (and (= post-enable "1") is-valid-post)
              (progn
                (if is-double
                  (progn
                    (setq pt-post-ins (polar p-center (- pt-dir (/ pi 2.0)) half-b))
                    (setq post-rot (+ pt-dir pi))
                  )
                  (progn
                    ;; [1H 확장] 버팀보 측면 x 보강재 플랜지선 교차 모서리에 밀착 (4방향 선택)
                    (setq post-pos-1h (tsp-brace-opt-get s-brace-opt 12 "0"))
                    (setq post-pp (tsp-1h-post-place p-center strut-ang half-b b post-pos-1h))
                    (setq pt-post-ins (car post-pp) post-rot (cadr post-pp) post-ys-1h (caddr post-pp))
                  )
                )

                (setq y_scale 1.0)
                (if (not is-double) (setq y_scale post-ys-1h))

                (if (and is-double (= post-side "R"))
                  (progn
                    (setq post-rot (- (* 2.0 pt-dir) post-rot))
                    (setq dist-to-ins (distance p-center pt-post-ins))
                    (setq ang-to-ins (angle p-center pt-post-ins))
                    (setq mirror-ang (- (* 2.0 pt-dir) ang-to-ins))
                    (setq pt-post-ins (polar p-center mirror-ang dist-to-ins))
                    (setq y_scale -1.0)
                  )
                )

                (if is-double
                  (progn
                    ;; [2H] 배열 모드 중 P키 실시간 왼쪽(L)<->오른쪽(R) 토글을 위해 기준 교차점/각도/반폭을 XDATA로 기록
                    (if (not (tblsearch "APPID" "TSP_POST_2H")) (regapp "TSP_POST_2H"))
                    (entmake
                      (list
                        '(0 . "INSERT")
                        (cons 2 post-block)
                        (cons 8 "_중간말뚝(Post pile)")
                        (cons 62 256)
                        (cons 10 pt-post-ins)
                        '(41 . 1.0)
                        (cons 42 y_scale)
                        '(43 . 1.0)
                        (cons 50 post-rot)
                        (list -3 (list "TSP_POST_2H"
                                   (cons 1010 p-center)
                                   (cons 1040 (float pt-dir))
                                   (cons 1040 (float half-b))))))
                  )
                  (progn
                    ;; [1H] AWSD 실시간 재배치를 위해 기준 교차점/각도/폭을 XDATA로 기록
                    (if (not (tblsearch "APPID" "TSP_POST_1H")) (regapp "TSP_POST_1H"))
                    (entmake
                      (list
                        '(0 . "INSERT")
                        (cons 2 post-block)
                        (cons 8 "_중간말뚝(Post pile)")
                        (cons 62 256)
                        (cons 10 pt-post-ins)
                        '(41 . 1.0)
                        (cons 42 y_scale)
                        '(43 . 1.0)
                        (cons 50 post-rot)
                        (list -3 (list "TSP_POST_1H"
                                   (cons 1010 p-center)
                                   (cons 1040 (float strut-ang))
                                   (cons 1040 (float half-b))
                                   (cons 1040 (float b))))))
                  )
                )
              )
            )

            (setq spatial-idx (1+ spatial-idx))
          )
        )
      )
    )
  )

  ;; 5. 방금 새로 그려진 객체들의 ID를 수집하여 반환
      (setq new-ents '())
      (setq nxt start-ent)

      (while (setq nxt (entnext nxt))
        (setq new-ents (append new-ents (list nxt)))
      )

      (princ "\n[성공] 버팀보 작도 및 중간말뚝 배치가 완료되었습니다.")
      new-ents
    )
)
