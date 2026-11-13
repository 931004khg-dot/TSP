;;; ==========================================================================
;;; 파일명: TSP_4D_Plan.lsp
;;; 구  분: 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part D)
;;; 역  할: 평면 띠장 오프셋/상향, H-Pile/토류판 배치, 사보강재/화타쐐기, 차수그라우팅, 연속 CIP 평면 배열, 세그먼트/모서리 번호표 및 수동말뚝 삭제 [tsp-draw-closed-poly, tsp-draw-wedge-connection, get-poly-segs]
;;; 포함 함수: tsp-draw-closed-poly, tsp-draw-wedge-connection, get-poly-segs
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-draw-closed-poly : 폴리라인 작도 범용 유틸리티 (모듈화 분리)
;;;;==========================================================================
(defun tsp-draw-closed-poly (pts layer color / lst p)
  (setq lst (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer) (cons 62 color) (cons 90 (length pts)) '(70 . 1)))
  (foreach p pts (setq lst (append lst (list (list 10 (car p) (cadr p))))))
  (entmake lst)
)

;;;;==========================================================================
;;;; defun tsp-draw-wedge-connection : 화타쐐기 작도 및 가림 판정용 다각형 외곽선 반환
;;;;==========================================================================
(defun tsp-draw-wedge-connection (pt_inner p1_flange p2_flange strut_dir wale_dir wale_norm conn_type /
                                  p1_wall p2_wall vW dot pExt_wall pOth_wall isExt2
                                  p1 p2 p3 fill_1 fill_2 dist_f1 dist_f2 half_bp
                                  wp_1 wp_2 wp_3 wp_4 ep_b1 ep_b2 ep_t1 ep_t2
                                  p1_foot_temp p1_temp l2_p2_temp p2_temp p2_foot_temp
                                  dir_to_p2 dir_to_p1 p2_foot p1_foot
                                  p1_14 p2_14 p1_64 p2_64 p_cut p2_end p3_end br_p1 br_p2 wedge_segs get-poly-segs
                                  pl_p1 pl_p2 pl_p3 pl_p4 ep_base1 ep_base2
                                  strut_width plate_width half_pw)

  ;; 다각형 정점들로부터 닫힌 선분 리스트를 추출하는 내부 함수
  (defun get-poly-segs (pts / segs i)
    (setq segs '() i 0)
    (while (< i (length pts))
      (setq segs (append segs (list (list (nth i pts) (nth (if (= i (1- (length pts))) 0 (1+ i)) pts)))))
      (setq i (1+ i))
    )
    segs
  )

  (setq wedge_segs '())
  ;; 1. 사보강재 진행 방향(isExt2) 판별 로직
  (setq p1_wall (inters p1_flange (polar p1_flange strut_dir 100.0) pt_inner (polar pt_inner wale_dir 100.0) nil))
  (setq p2_wall (inters p2_flange (polar p2_flange strut_dir 100.0) pt_inner (polar pt_inner wale_dir 100.0) nil))
  (setq vW (list (- (car p2_wall) (car p1_wall)) (- (cadr p2_wall) (cadr p1_wall))))
  (setq dot (+ (* (car vW) (cos strut_dir)) (* (cadr vW) (sin strut_dir))))
  (if (> dot 0)
    (setq isExt2 T pExt_wall p2_wall pOth_wall p1_wall)
    (setq isExt2 nil pExt_wall p1_wall pOth_wall p2_wall)
  )

  ;; 3-0. [마감철판 직결] 가변 마감철판 작도
  (if (or (null conn_type) (= conn_type "") (= conn_type "마감철판 직결"))
    (progn
      ;; 부재 폭(strut_width) 계산: 두 플랜지점 간의 거리에 각도차의 사인값을 곱하여 수직 투영 폭 도출
      (setq strut_width (* (distance p1_flange p2_flange) (abs (sin (- (angle p1_flange p2_flange) strut_dir)))))

      ;; 철판 너비(plate_width) 계산: 최소 500, 그 이상은 폭 + 200
      (setq plate_width (max 500.0 (+ strut_width 200.0)))
      (setq half_pw (/ plate_width 2.0))

      ;; 계산된 절반 폭(half_pw)을 적용하여 철판 외곽선 꼭짓점 산출
      (setq pl_p1 (polar pt_inner wale_dir half_pw))
      (setq pl_p2 (polar pt_inner (+ wale_dir pi) half_pw))
      (setq pl_p3 (polar pl_p2 wale_norm 14.0))
      (setq pl_p4 (polar pl_p1 wale_norm 14.0))

      (create-layer-if-not-exists "_철판(Plate)" "2")
      (tsp-draw-closed-poly (list pl_p1 pl_p2 pl_p3 pl_p4) "_철판(Plate)" 2)
      (setq wedge_segs (append wedge_segs (get-poly-segs (list pl_p1 pl_p2 pl_p3 pl_p4))))

      ;; 철판 윗면(pl_p3~pl_p4)과 까치발 플랜지선의 교차점 산출
      (setq ep_base1 (inters p1_flange (polar p1_flange strut_dir 100.0) pl_p3 pl_p4 nil))
      (setq ep_base2 (inters p2_flange (polar p2_flange strut_dir 100.0) pl_p3 pl_p4 nil))
      (if (null ep_base1) (setq ep_base1 (polar p1_flange strut_dir 14.0)))
      (if (null ep_base2) (setq ep_base2 (polar p2_flange strut_dir 14.0)))
      (if isExt2
        (setq br_p1 ep_base2 br_p2 ep_base1)
        (setq br_p1 ep_base1 br_p2 ep_base2)
      )
    )
    (progn
    ;; 3. 연결 방식(conn_type)에 따른 작도 및 다각형 추출 분기
    (if (= conn_type "화타쐐기 없음")
      (progn
        (setq p1 pt_inner)
        (setq p2 (polar p1 wale_norm 14.0))

        (setq fill_1 (inters p1_flange (polar p1_flange strut_dir 100.0) p2 (polar p2 wale_dir 100.0) nil))
        (setq fill_2 (inters p2_flange (polar p2_flange strut_dir 100.0) p2 (polar p2 wale_dir 100.0) nil))

        (setq dist_f1 (distance p2 fill_1))
        (setq dist_f2 (distance p2 fill_2))
        (setq half_bp (max 250.0 (+ (max dist_f1 dist_f2) 50.0)))

        (setq wp_3 (polar p2 (+ wale_dir pi) half_bp))
        (setq wp_4 (polar p2 wale_dir half_bp))
        (setq wp_1 (polar wp_4 (+ wale_norm pi) 14.0))
        (setq wp_2 (polar wp_3 (+ wale_norm pi) 14.0))
        (tsp-draw-closed-poly (list wp_1 wp_2 wp_3 wp_4) "_화타쐐기(Wedge)" 2)
        (setq wedge_segs (append wedge_segs (get-poly-segs (list wp_1 wp_2 wp_3 wp_4))))

        (setq p3 (polar p2 strut_dir 220.0))
        (setq ep_b1 (inters p3 (polar p3 (+ strut_dir (/ pi 2.0)) 100.0) p1_flange (polar p1_flange strut_dir 100.0) nil))
        (setq ep_b2 (inters p3 (polar p3 (+ strut_dir (/ pi 2.0)) 100.0) p2_flange (polar p2_flange strut_dir 100.0) nil))

        (tsp-draw-closed-poly (list ep_b1 ep_b2 fill_2 fill_1) "_화타쐐기(Wedge)" 2)
        (setq wedge_segs (append wedge_segs (get-poly-segs (list ep_b1 ep_b2 fill_2 fill_1))))

        (setq ep_t1 (polar ep_b1 strut_dir 14.0))
        (setq ep_t2 (polar ep_b2 strut_dir 14.0))
        (tsp-draw-closed-poly (list ep_b1 ep_b2 ep_t2 ep_t1) "_화타쐐기(Wedge)" 2)
        (setq wedge_segs (append wedge_segs (get-poly-segs (list ep_b1 ep_b2 ep_t2 ep_t1))))

        (if isExt2
          (setq br_p1 ep_t2 br_p2 ep_t1)
          (setq br_p1 ep_t1 br_p2 ep_t2)
        )
      )
      (progn
        (setq p1_foot_temp pOth_wall)
        (setq p1_temp (polar p1_foot_temp wale_norm 64.0))
        (setq l2_p2_temp (polar p1_temp (angle pOth_wall pExt_wall) 100.0))
        (setq p2_temp (inters p1_temp l2_p2_temp pExt_wall (polar pExt_wall strut_dir 100.0) nil))
        (setq p2_foot_temp (polar p2_temp (+ wale_norm pi) 64.0))

        (setq dir_to_p2 (angle pt_inner p2_foot_temp))
        (setq dir_to_p1 (+ dir_to_p2 pi))
        (setq p2_foot (polar pt_inner dir_to_p2 (distance pt_inner p2_foot_temp)))
        (setq p1_foot (polar pt_inner dir_to_p1 (distance pt_inner p2_foot_temp)))

        (setq p1_14 (polar p1_foot wale_norm 14.0))
        (setq p1_64 (polar p1_foot wale_norm 64.0))
        (setq p2_14 (polar p2_foot wale_norm 14.0))
        (setq p2_64 p2_temp)

        (setq p_cut (inters p2_64 (polar p2_64 (+ strut_dir (/ pi 2.0)) 100.0) pOth_wall (polar pOth_wall strut_dir 100.0) nil))
        (setq p2_end (polar p2_64 strut_dir 14.0))
        (setq p3_end (polar p_cut strut_dir 14.0))

        (tsp-draw-closed-poly (list p1_foot p2_foot p2_14 p1_14) "_화타쐐기(Wedge)" 2)
        (setq wedge_segs (append wedge_segs (get-poly-segs (list p1_foot p2_foot p2_14 p1_14))))

        (tsp-draw-closed-poly (list p1_14 p1_64 p_cut p2_64 p2_14) "_화타쐐기(Wedge)" 2)
        (setq wedge_segs (append wedge_segs (get-poly-segs (list p1_14 p1_64 p_cut p2_64 p2_14))))

        (tsp-draw-closed-poly (list p2_64 p_cut p3_end p2_end) "_화타쐐기(Wedge)" 2)
        (setq wedge_segs (append wedge_segs (get-poly-segs (list p2_64 p_cut p3_end p2_end))))

        (if isExt2
          (setq br_p1 p3_end br_p2 p2_end)
          (setq br_p1 p2_end br_p2 p3_end)
        )
      )
    )
    )
  )

  ;; 잭 삽입점 및 가림 판정용 다각형 선분 동시 반환
  (list br_p1 br_p2 wedge_segs)
)
