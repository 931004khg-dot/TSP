;;; ==========================================================================
;;; 파일명: TSP_4A_Plan.lsp
;;; 구  분: 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part A)
;;; 역  할: 평면 띠장 오프셋/상향, H-Pile/토류판 배치, 사보강재/화타쐐기, 차수그라우팅, 연속 CIP 평면 배열, 세그먼트/모서리 번호표 및 수동말뚝 삭제 [create-wale-offsets, make-wale-lw, get-corner-upg-vals, get-corner-splice-length, draw-wale-subsegment]
;;; 포함 함수: create-wale-offsets, make-wale-lw, get-corner-upg-vals, get-corner-splice-length, draw-wale-subsegment
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;; ==========================================================================
;;; [SECTION 5] 평면도 작도 로직 (Plan View Logic)
;;; ==========================================================================

;;;;==========================================================================
;;;; defun create-wale-offsets : 각 세그먼트 데이터에 맞춰 띠장(Wale)생성 (사전 검사 동기화 및 마감 완벽 적용)
;;;;==========================================================================
(defun create-wale-offsets (boundary-ent boundary-orient / seg-idx max-idx seg-data is-def v-start v-end v-angle wale-spec h b tw tf wale-offset-sign offset-dir next-idx next-data next-def next-start next-angle prev-idx prev-data prev-def prev-end prev-angle diff-start diff-end int-angle-start int-angle-end is-butt-start is-0-start is-butt-end is-0-end cut-start-p1 cut-start-p2 cut-end-p1 cut-end-p2 p1 p2 p-outer-s p-outer-e p-of-s p-of-e p-if-s p-if-e p-inner-s p-inner-e p-inner-corner prev-offset-dir prev-wale-spec prev-wale-vals h-prev limit-angle upg-s-vals u-h-s u-tf-s L-start upg-e-vals u-h-e u-tf-e L-end seg-length eff-h-start eff-h-end h-prev-base eff-h-prev next-wale-spec next-wale-vals h-next-base eff-h-next next-offset-dir p-splice1 p-splice2 splice1-cut-p1 splice1-cut-p2 splice2-cut-p1 splice2-cut-p2 res-start res-base res-end base-start-cut base-start-cut-p2 base-end-cut base-end-cut-p2 get-corner-upg-vals get-corner-splice-length draw-wale-subsegment make-wale-lw wale-vals is-upgraded-any ctc-mm max-u-h max-u-tf wall-spec hp-b cip-dia-val cip-overlap-val cip-mode cip-base-ctc interval-idx cip-interval wall-type true-h-list true-tf-list t-idx t-data t-h t-tf t-spec t-vals t-upg-s t-next-idx t-upg-e t-L-start t-L-end t-wall-type t-ctc t-len corner-s-h corner-e-h p-splice1-upg p-splice1-base p-splice2-upg p-splice2-base splice1-upg-cut-p1 splice1-upg-cut-p2 splice1-base-cut-p1 splice1-base-cut-p2 splice2-upg-cut-p1 splice2-upg-cut-p2 splice2-base-cut-p1 splice2-base-cut-p2 cip-h-idx wall-vals exp-pairs exp-list exp-parent exp-idx p-idx prev-p-idx next-p-idx next-corner-idx is-first-sub is-last-sub wrap-first-data exp-old-turn-tol)

;;;;==========================================================================
;;;; defun make-wale-lw : [보조 1] LWPOLYLINE 강제 생성
;;;;==========================================================================
  (defun make-wale-lw (p1 p2 layer color)
    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                   (cons 8 layer) (cons 62 color) '(90 . 2) '(70 . 0)
                   (list 10 (car p1) (cadr p1)) (list 10 (car p2) (cadr p2))))
  )

;;;;==========================================================================
;;;; defun get-corner-upg-vals : [보조 2] 코너의 상향(Upgrade) 띠장 데이터 추출
;;;;==========================================================================
  (defun get-corner-upg-vals (corner-idx / c-seg c-upg)
    (if (and (>= corner-idx 0) (< corner-idx (length *segment-list*)))
      (progn
        (setq c-seg (nth corner-idx *segment-list*))
        (setq c-upg (cdr (assoc 'UPGRADE-WALE c-seg)))
        (if (and c-upg (= (car c-upg) "Y"))
          (parse-h-spec (cadr c-upg))
          nil
        )
      )
      nil
    )
  )
  ;; [보조 3] 코너의 보강 띠장 설치 길이(Splice Length) 자동 계산
;;;;==========================================================================
;;;; defun get-corner-splice-length : 외부 H-Pile 탐색 및 수동 선택 범위 처리 로직 통합
;;;;==========================================================================
(defun get-corner-splice-length (corner-idx v-start v-angle / c-seg c-brace max-l b-dist b-h min-limit d-dist perp-dist best-d best-pt wall-spec wall-vals hp-b far-edge wall-type cip-h-idx seg-len spec-str brace-edge c-upg user-upg-pts manual-L pt dx dy pt-d pt-perp)
  (if (and (>= corner-idx 0) (< corner-idx (length *segment-list*)))
    (progn
      (setq c-seg (nth corner-idx *segment-list*))
      (setq seg-len (cdr (assoc 'LENGTH c-seg)))

      (setq wall-type (cdr (assoc 'WALL-TYPE c-seg)))
      (if (= wall-type "CIP")
        (progn
          (setq cip-h-idx (atoi (cdr (assoc 'CIP-HPILE-IDX c-seg))))
          (setq wall-vals (parse-h-spec (nth cip-h-idx *tsp-std-wall-list*)))
        )
        (progn
          (setq wall-spec (cdr (assoc 'WALL-SPEC c-seg)))
          (if (= wall-spec "User-defined")
            (setq wall-vals (cdr (assoc 'WALL-CUSTOM c-seg)))
            (setq wall-vals (parse-h-spec wall-spec))
          )
        )
      )
      (if (null wall-vals) (setq wall-vals '(300 300 10 15)))
      (setq hp-b (nth 1 wall-vals))

      ;; 사용자가 도면에서 수동으로 선택한 H-pile(UPG-LIMIT-PTS)이 있는지 먼저 검사
      (setq c-upg (cdr (assoc 'UPGRADE-WALE c-seg)))
      (setq user-upg-pts (if (and c-upg (> (length c-upg) 2)) (nth 2 c-upg) '()))
      (setq manual-L nil)

      (if user-upg-pts
        (foreach pt-data user-upg-pts
          (setq pt (cadr pt-data))
          (setq dx (- (car pt) (car v-start)))
          (setq dy (- (cadr pt) (cadr v-start)))
          ;; 벡터 내적 및 외적(거리)을 통해 현재 방향(v-angle)에 해당하는 포인트를 식별
          (setq pt-d (+ (* dx (cos v-angle)) (* dy (sin v-angle))))
          (setq pt-perp (abs (- (* dy (cos v-angle)) (* dx (sin v-angle)))))

          (if (and (> pt-d 0) (< pt-perp 1000.0))
            (setq manual-L pt-d)
          )
        )
      )

      ;; 사보강재 최소 필요 길이(auto-L)를 항상 먼저 계산하여 manual-L이 사보강재보다 짧게 잡히는 현상 원천 차단
      (setq c-brace (cdr (assoc 'CORNER-BRACE c-seg)))
      (setq max-l 0.0)
      (if (and c-brace (= (car c-brace) "Y") (cadr c-brace))
        (foreach b-row (cadr c-brace)
          (setq b-dist (* (nth 2 b-row) 1000.0))
          (setq b-h (nth 0 (cadr b-row)))
          (setq spec-str (nth 0 b-row))

          ;; 맞버팀대(2H) 및 45도 사선 결합 투영폭(/ b-h 2 * sin45 = 0.7071 * b-h) 반영
          (setq brace-edge
            (if (wcmatch (strcase spec-str) "2H*")
              (* b-h 2.1213) ; 2H 사선 투영폭 여유 반영
              (* b-h 0.7071) ; 1H 45도 사선 투영 반폭 (150 * sqrt(2) = 212.13mm)
            )
          )

          (if (> (+ b-dist brace-edge) max-l)
            (setq max-l (+ b-dist brace-edge))
          )
        )
      )
      (setq min-limit (if (> max-l 0) (+ max-l 150.0) 0.0))
      (setq auto-L 0.0)
      (if (> min-limit 0.0)
        (progn
          (setq best-d 1e99 best-pt nil)
          (if *tsp-hpile-points*
            (foreach p *tsp-hpile-points*
              (setq d-dist (+ (* (- (car p) (car v-start)) (cos v-angle))
                              (* (- (cadr p) (cadr v-start)) (sin v-angle))))
              (setq far-edge (+ d-dist (/ hp-b 2.0)))
              (setq perp-dist (abs (+ (* (- (car p) (car v-start)) (- (sin v-angle)))
                                    (* (- (cadr p) (cadr v-start)) (cos v-angle)))))

              (if (and (> far-edge min-limit) (< perp-dist 500.0) (> d-dist 0.0))
                (if (< d-dist best-d)
                  (setq best-d d-dist best-pt p)
                )
              )
            )
          )
          (setq auto-L (if (< best-d 1e90) best-d seg-len))
        )
      )
      (if manual-L
        (max manual-L auto-L)
        auto-L
      )
    )
    0.0
  )
)

;;;;==========================================================================
;;;; defun draw-wale-subsegment : [보조 4] 지정된 도막(Sub-segment)의 띠장 라인 4줄 작도
;;;;==========================================================================
  (defun draw-wale-subsegment (p-start p-end cut-s-p1 cut-s-p2 cut-e-p1 cut-e-p2 sub-h sub-tf / p1_outer p2_outer p_outer_s p_outer_e p1_of p2_of p_of_s p_of_e p1_if p2_if p_if_s p_if_e p1_inner p2_inner p_inner_s p_inner_e)
    (if (> (distance p-start p-end) 0.01)
      (progn
        (setq p1_outer (polar p-start offset-dir 0))
        (setq p2_outer (polar p-end offset-dir 0))
        (setq p_outer_s (tsp-get-intersection-point p1_outer p2_outer cut-s-p1 cut-s-p2))
        (setq p_outer_e (tsp-get-intersection-point p1_outer p2_outer cut-e-p1 cut-e-p2))
        (setq p1_of (polar p-start offset-dir sub-tf))
        (setq p2_of (polar p-end offset-dir sub-tf))
        (setq p_of_s (tsp-get-intersection-point p1_of p2_of cut-s-p1 cut-s-p2))
        (setq p_of_e (tsp-get-intersection-point p1_of p2_of cut-e-p1 cut-e-p2))
        (setq p1_if (polar p-start offset-dir (- sub-h sub-tf)))
        (setq p2_if (polar p-end offset-dir (- sub-h sub-tf)))
        (setq p_if_s (tsp-get-intersection-point p1_if p2_if cut-s-p1 cut-s-p2))
        (setq p_if_e (tsp-get-intersection-point p1_if p2_if cut-e-p1 cut-e-p2))
        (setq p1_inner (polar p-start offset-dir sub-h))
        (setq p2_inner (polar p-end offset-dir sub-h))
        (setq p_inner_s (tsp-get-intersection-point p1_inner p2_inner cut-s-p1 cut-s-p2))
        (setq p_inner_e (tsp-get-intersection-point p1_inner p2_inner cut-e-p1 cut-e-p2))
        (if (null p_outer_s) (setq p_outer_s p1_outer))
        (if (null p_outer_e) (setq p_outer_e p2_outer))
        (if (null p_inner_s) (setq p_inner_s p1_inner))
        (if (null p_inner_e) (setq p_inner_e p2_inner))
        (if (null p_of_s) (setq p_of_s p1_of))
        (if (null p_of_e) (setq p_of_e p2_of))
        (if (null p_if_s) (setq p_if_s p1_if))
        (if (null p_if_e) (setq p_if_e p2_if))
        (make-wale-lw p_outer_s p_outer_e "_띠장(wale)" 3)
        (make-wale-lw p_of_s p_of_e "_띠장(wale)" 1)
        (make-wale-lw p_if_s p_if_e "_띠장(wale)" 1)
        (make-wale-lw p_inner_s p_inner_e "_띠장(wale)" 3)
        (list p_outer_s p_inner_s p_outer_e p_inner_e)
      )
      nil
    )
  )

  ;; 1. 레이어 설정 및 초기화
  (create-layer-if-not-exists "_띠장(wale)" "3")
  (setq wale-offset-sign boundary-orient)
  (setq max-idx (length *segment-list*))
  (setq is-upgraded-any nil)
  (princ "\n[작도] 띠장(Wale) 작도 처리 시작...")

  ;; --- [사전 검사 (Pre-Pass)] ---
  ;; 전체 세그먼트의 '최종 베이스 규격(True H, TF)'을 미리 계산하여 이웃 간 절단면 기하학 불일치 원천 차단
  (setq true-h-list '() true-tf-list '())
  (setq t-idx 0)
  (while (< t-idx max-idx)
    (setq t-data (nth t-idx *segment-list*))
    (setq t-h 300 t-tf 15)
    (if (cdr (assoc 'IS-DEFINED t-data))
      (progn
        (setq t-spec (cdr (assoc 'WALE-SPEC t-data)))
        (setq t-vals (if (= t-spec "User-defined") (cdr (assoc 'WALE-CUSTOM t-data)) (parse-h-spec t-spec)))
        (if t-vals (setq t-h (nth 0 t-vals) t-tf (nth 3 t-vals)))
        ;; [지보재 띠장 우선] 지보재(SUPPORT-LIST)가 존재하면 지보재들에 설정된 띠장 규격 중 최대값을 베이스 규격으로 우선 채택
        (if (cdr (assoc 'SUPPORT-LIST t-data))
          (progn
            (setq sup-max-h 0 sup-max-tf 0)
            (foreach s-it (cdr (assoc 'SUPPORT-LIST t-data))
              (setq s-wv (tsp-get-support-item-wale-vals s-it t-spec (cdr (assoc 'WALE-CUSTOM t-data))))
              (if (> (nth 0 s-wv) sup-max-h)
                (setq sup-max-h (nth 0 s-wv) sup-max-tf (nth 3 s-wv))
              )
            )
            (if (> sup-max-h 0) (setq t-h sup-max-h t-tf sup-max-tf))
          )
        )

        (setq t-upg-s (get-corner-upg-vals t-idx))
        (setq t-next-idx (if (is-closed-polyline boundary-ent) (rem (1+ t-idx) max-idx) (1+ t-idx)))
        (setq t-upg-e (get-corner-upg-vals t-next-idx))

        (setq t-L-start (if t-upg-s (get-corner-splice-length t-idx (cdr (assoc 'V-START t-data)) (cdr (assoc 'ANGLE t-data))) 0.0))
        (setq t-L-end (if t-upg-e (get-corner-splice-length t-next-idx (cdr (assoc 'V-END t-data)) (+ (cdr (assoc 'ANGLE t-data)) pi)) 0.0))

        (setq t-wall-type (cdr (assoc 'WALL-TYPE t-data)))
        (if (= t-wall-type "CIP")
          (progn
            (setq cip-dia-val (atof (cdr (assoc 'CIP-DIA t-data))))
            (setq cip-overlap-val (atof (cdr (assoc 'CIP-OVERLAP t-data))))
            (setq cip-mode (cdr (assoc 'CIP-MODE-IDX t-data)))
            (setq cip-base-ctc (if (= cip-mode "0") cip-dia-val (- cip-dia-val cip-overlap-val)))
            (setq interval-idx (cdr (assoc 'CIP-INTERVAL-IDX t-data)))
            (setq cip-interval (cond ((= interval-idx "1") 2.0) ((= interval-idx "2") 3.0) (t 1.0)))
            (setq t-ctc (* cip-base-ctc cip-interval))
          )
          (setq t-ctc (* (if (cdr (assoc 'CTC t-data)) (cdr (assoc 'CTC t-data)) 2.0) 1000.0))
        )

        (setq t-len (cdr (assoc 'LENGTH t-data)))
        ;; CTC 기준 전체 상향 판단
        (if (and (or (> t-L-start 0.0) (> t-L-end 0.0)) (< (- t-len (+ t-L-start t-L-end)) (* t-ctc 1.5)))
          (progn
            (if t-upg-s (if (> (nth 0 t-upg-s) t-h) (setq t-h (nth 0 t-upg-s) t-tf (nth 3 t-upg-s))))
            (if t-upg-e (if (> (nth 0 t-upg-e) t-h) (setq t-h (nth 0 t-upg-e) t-tf (nth 3 t-upg-e))))
            (setq is-upgraded-any T)
          )
        )
      )
    )
    (setq true-h-list (append true-h-list (list t-h)))
    (setq true-tf-list (append true-tf-list (list t-tf)))
    (setq t-idx (1+ t-idx))
  )
  ;; ------------------------------------------

    ;; 2. 메인 작도: 전체 세그먼트 순회
  ;; [B안] 경계선 추종: 설계 세그먼트를 경계선 실제 형상(자동삭제된 중간 180도 근접 꺾임 포함)에
  ;;   따라 소구간으로 전개하여, 띠장이 삭제된 모서리를 무시한 현(弦)으로 그려지는 것을 막는다.
  ;;   - 경계선 정보 해석 실패 시 tsp-expand-segments-by-geometry가 원본 리스트를 그대로
  ;;     반환하므로 기존(현 기준) 방식과 동일하게 동작한다. (하위호환 / 기존 기능 보존)
  ;;   - 중간 꺾임이 없는 일반 세그먼트는 전개 결과가 원본과 동일하여 작도 결과 불변.
  ;;   - 중간 꺾임점 판정 허용오차는 자동 모서리 삭제 규칙과 동일한 5도를 적용.
  (setq exp-old-turn-tol (if (boundp '*tsp-geo-follow-min-turn-deg*) *tsp-geo-follow-min-turn-deg* nil))
  (setq *tsp-geo-follow-min-turn-deg* 5.0)
  (setq exp-pairs (tsp-expand-segments-by-geometry *segment-list* boundary-ent))
  (if exp-old-turn-tol
    (setq *tsp-geo-follow-min-turn-deg* exp-old-turn-tol)
    (setq *tsp-geo-follow-min-turn-deg* nil)
  )
  (setq exp-list (car exp-pairs) exp-parent (cadr exp-pairs))
  (if (or (null exp-list) (null exp-parent) (/= (length exp-list) (length exp-parent)))
    (progn
      (setq exp-list *segment-list*)
      (setq exp-parent '())
      (setq exp-idx 0)
      (while (< exp-idx (length exp-list))
        (setq exp-parent (append exp-parent (list exp-idx)))
        (setq exp-idx (1+ exp-idx))
      )
    )
  )
  (setq max-idx (length exp-list))
  (setq exp-idx 0)
  (while (< exp-idx max-idx)
    (setq seg-data (nth exp-idx exp-list))
    (setq p-idx (nth exp-idx exp-parent))
    (setq is-def (cdr (assoc 'IS-DEFINED seg-data)))
    (if is-def
      (progn
        ;; [STEP 1] 현재 세그먼트 기본 파싱
        (setq v-start (cdr (assoc 'V-START seg-data)))
        (setq v-end (cdr (assoc 'V-END seg-data)))
        (setq v-angle (cdr (assoc 'ANGLE seg-data)))
        (setq seg-length (distance v-start v-end))
        (setq offset-dir (+ v-angle (* (/ pi 2.0) wale-offset-sign)))

        (setq wall-type (cdr (assoc 'WALL-TYPE seg-data)))
        (if (= wall-type "CIP")
          (progn
            (setq cip-h-idx (atoi (cdr (assoc 'CIP-HPILE-IDX seg-data))))
            (setq wall-vals (parse-h-spec (nth cip-h-idx *tsp-std-wall-list*)))
          )
          (progn
            (setq wall-spec (cdr (assoc 'WALL-SPEC seg-data)))
            (if (= wall-spec "User-defined")
              (setq wall-vals (cdr (assoc 'WALL-CUSTOM seg-data)))
              (setq wall-vals (parse-h-spec wall-spec))
            )
          )
        )
        (if (null wall-vals) (setq wall-vals '(300 300 10 15)))
        (setq hp-b (nth 1 wall-vals))

        ;; Pre-Pass에서 도출된 현재 구간의 규격(True H) 적용
                (setq h (nth p-idx true-h-list) tf (nth p-idx true-tf-list))

        ;; [STEP 2] 동기화된 인접 규격 및 보강 도막(Splice) 계산
                (setq next-idx (if (is-closed-polyline boundary-ent) (rem (1+ exp-idx) max-idx) (1+ exp-idx)))
                (setq prev-idx (if (is-closed-polyline boundary-ent) (if (= exp-idx 0) (1- max-idx) (1- exp-idx)) (1- exp-idx)))

        ;; 열린 경계선의 경우 시작/끝점에서 인덱스가 -1 또는 max-idx를 벗어나 에러가 발생하는 것을 방지하는 안전 검사 추가
                (setq next-data (if (and (>= next-idx 0) (< next-idx max-idx)) (nth next-idx exp-list) nil))
                (setq prev-data (if (and (>= prev-idx 0) (< prev-idx max-idx)) (nth prev-idx exp-list) nil))

        (setq next-def (if next-data (cdr (assoc 'IS-DEFINED next-data)) nil))
        (setq prev-def (if prev-data (cdr (assoc 'IS-DEFINED prev-data)) nil))

        (setq next-start (if next-def (cdr (assoc 'V-START next-data)) nil))
        (setq next-angle (if next-def (cdr (assoc 'ANGLE next-data)) nil))
        (setq prev-end (if prev-def (cdr (assoc 'V-END prev-data)) nil))
        (setq prev-angle (if prev-def (cdr (assoc 'ANGLE prev-data)) nil))

        ;; 이웃 세그먼트의 True H 추출
                (setq h-prev-base (if prev-def (nth (nth prev-idx exp-parent) true-h-list) h))
                (setq h-next-base (if next-def (nth (nth next-idx exp-parent) true-h-list) h))

                ;; 모서리에 사용자가 명시한 보강재 규격 추출
        ;; [B안] 전개된 소구간 기준: 부모 설계 세그먼트의 시작/끝 '코너'에서만 상향 규격을 읽는다.
        ;;   (부모 내부의 중간 꺾임점은 코너가 아니므로 상향/사보강 대상에서 제외)
        (setq prev-p-idx (if (and (>= prev-idx 0) (< prev-idx (length exp-parent))) (nth prev-idx exp-parent) nil))
        (setq next-p-idx (if (and (>= next-idx 0) (< next-idx (length exp-parent))) (nth next-idx exp-parent) nil))
        (setq is-first-sub (or (= exp-idx 0) (/= prev-p-idx p-idx)))
        (setq is-last-sub (or (= exp-idx (1- max-idx)) (and next-p-idx (/= next-p-idx p-idx))))
        (setq next-corner-idx
          (cond
            ((= exp-idx (1- max-idx))
             (if (is-closed-polyline boundary-ent) (nth 0 exp-parent) nil))
            ((and next-p-idx (/= next-p-idx p-idx)) next-p-idx)
            (t nil)
          )
        )
        (setq upg-s-vals (if is-first-sub (get-corner-upg-vals p-idx) nil))
        (setq corner-s-h (if upg-s-vals (nth 0 upg-s-vals) 0))
        (setq upg-e-vals (if (and is-last-sub next-corner-idx) (get-corner-upg-vals next-corner-idx) nil))
        (setq corner-e-h (if upg-e-vals (nth 0 upg-e-vals) 0))

        ;; 절단면(Cutting Plane)의 최종 폭은 만나는 두 구간과 모서리 규격 중 '최대값'
        (setq eff-h-start (max h h-prev-base corner-s-h))
        (setq eff-h-prev eff-h-start)

        (setq eff-h-end (max h h-next-base corner-e-h))
        (setq eff-h-next eff-h-end)

        ;; 보강 띠장 도막(Splice) 생성 여부 결정
        ;; 내 구간 최종 베이스 폭(h)보다 모서리 필요 폭(eff-h)이 더 클 때만 돌출 도막 생성
        (if (> eff-h-start h)
          (progn
            (setq u-h-s eff-h-start)
            (setq u-tf-s (if (and upg-s-vals (= corner-s-h eff-h-start)) (nth 3 upg-s-vals) tf))
                        (setq L-start (get-corner-splice-length p-idx v-start v-angle))
          )
          (setq L-start 0.0 u-h-s nil)
        )

        (if (> eff-h-end h)
          (progn
            (setq u-h-e eff-h-end)
            (setq u-tf-e (if (and upg-e-vals (= corner-e-h eff-h-end)) (nth 3 upg-e-vals) tf))
                        ;; [B안] 폐합 경계선의 마지막 소구간 끝 코너(0번 코너)는 첫 소구간의 시작 기하로 측정
            (setq wrap-first-data
              (if (and (= exp-idx (1- max-idx)) (is-closed-polyline boundary-ent) next-corner-idx)
                (nth 0 exp-list)
                nil
              )
            )
            (setq L-end
              (get-corner-splice-length next-corner-idx
                (if wrap-first-data (cdr (assoc 'V-START wrap-first-data)) v-end)
                (if wrap-first-data (cdr (assoc 'ANGLE wrap-first-data)) (+ v-angle pi))
              )
            )
          )
          (setq L-end 0.0 u-h-e nil)
        )

        (if (or (> L-start 0.0) (> L-end 0.0))
          (setq is-upgraded-any T)
        )

        (if (> (+ L-start L-end) seg-length)
          (progn
            (setq L-start (min L-start (/ seg-length 2.0)))
            (setq L-end (min L-end (/ seg-length 2.0)))
          )
        )

        ;; [STEP 3] 시작점 절단면 계산 (eff-h 기준)
        (setq is-butt-start nil is-0-start nil)
        (setq limit-angle (* 120.0 (/ pi 180.0)))
        (if prev-def
          (progn
            (setq diff-start (abs (- v-angle prev-angle)))
            (while (> diff-start pi) (setq diff-start (abs (- diff-start (* 2.0 pi)))))
            (setq int-angle-start (- pi diff-start))
            (if (< diff-start 0.01) (setq is-0-start T))
            (if (and (>= diff-start 0.01) (<= int-angle-start (+ limit-angle 0.01))) (setq is-butt-start T))
          )
        )
        (cond
          ((or (not prev-def) is-0-start)
           (setq cut-start-p1 v-start)
           (setq cut-start-p2 (polar v-start offset-dir 100.0))
          )
          (is-butt-start
           (setq prev-offset-dir (+ prev-angle (* (/ pi 2.0) wale-offset-sign)))
           (setq cut-start-p1 (polar v-start prev-offset-dir eff-h-prev))
           (setq cut-start-p2 (polar cut-start-p1 prev-angle 100.0))
          )
          (t
           (setq prev-offset-dir (+ prev-angle (* (/ pi 2.0) wale-offset-sign)))
           (setq p-inner-corner (tsp-get-intersection-point
                                  (polar v-start offset-dir eff-h-start) (polar v-end offset-dir eff-h-start)
                                  (polar prev-end prev-offset-dir eff-h-prev)
                                  (polar (cdr (assoc 'V-START prev-data)) prev-offset-dir eff-h-prev)))
           (if (null p-inner-corner) (setq p-inner-corner (polar v-start offset-dir eff-h-start)))
           (setq cut-start-p1 v-start)
           (setq cut-start-p2 p-inner-corner)
          )
        )
        ;; [STEP 4] 끝점 절단면 계산 (eff-h 기준)
        (setq is-butt-end nil is-0-end nil)
        (if next-def
          (progn
            (setq diff-end (abs (- next-angle v-angle)))
            (while (> diff-end pi) (setq diff-end (abs (- diff-end (* 2.0 pi)))))
            (setq int-angle-end (- pi diff-end))
            (if (< diff-end 0.01) (setq is-0-end T))
            (if (and (>= diff-end 0.01) (<= int-angle-end (+ limit-angle 0.01))) (setq is-butt-end T))
          )
        )
        (cond
          ((or (not next-def) is-0-end)
           (setq cut-end-p1 v-end)
           (setq cut-end-p2 (polar v-end offset-dir 100.0))
          )
          (is-butt-end
           (setq cut-end-p1 v-end)
           (setq cut-end-p2 (polar v-end next-angle 100.0))
          )
          (t
           (setq next-offset-dir (+ next-angle (* (/ pi 2.0) wale-offset-sign)))
           (setq p-inner-corner (tsp-get-intersection-point
                                  (polar v-start offset-dir eff-h-end) (polar v-end offset-dir eff-h-end)
                                  (polar next-start next-offset-dir eff-h-next)
                                  (polar (cdr (assoc 'V-END next-data)) next-offset-dir eff-h-next)))
           (if (null p-inner-corner) (setq p-inner-corner (polar v-end offset-dir eff-h-end)))
           (setq cut-end-p1 v-end)
           (setq cut-end-p2 p-inner-corner)
          )
        )
        ;; [STEP 5] Splice 절단면 계산 (직각 절단 및 동적 플랜지 너비 오버랩 적용)
        (if (> L-start 0.0)
          (progn
            (setq p-splice1-upg (polar v-start v-angle (+ L-start (/ hp-b 2.0))))
            (setq p-splice1-base (polar v-start v-angle (- L-start (/ hp-b 2.0))))
          )
          (progn
            (setq p-splice1-upg v-start)
            (setq p-splice1-base v-start)
          )
        )
        (if (> L-end 0.0)
          (progn
            (setq p-splice2-upg (polar v-end (+ v-angle pi) (+ L-end (/ hp-b 2.0))))
            (setq p-splice2-base (polar v-end (+ v-angle pi) (- L-end (/ hp-b 2.0))))
          )
          (progn
            (setq p-splice2-upg v-end)
            (setq p-splice2-base v-end)
          )
        )

        (setq splice1-upg-cut-p1 p-splice1-upg)
        (setq splice1-upg-cut-p2 (polar p-splice1-upg offset-dir 100.0))
        (setq splice1-base-cut-p1 p-splice1-base)
        (setq splice1-base-cut-p2 (polar p-splice1-base offset-dir 100.0))

        (setq splice2-upg-cut-p1 p-splice2-upg)
        (setq splice2-upg-cut-p2 (polar p-splice2-upg offset-dir 100.0))
        (setq splice2-base-cut-p1 p-splice2-base)
        (setq splice2-base-cut-p2 (polar p-splice2-base offset-dir 100.0))

        ;; [STEP 6] 3도막(Sub-segments) 작도
        (setq res-start nil res-base nil res-end nil)

        ;; 6.1 Start Upgrade Wale (상향 띠장: 플랜지 너비의 절반만큼 더 연장)
        (if (> L-start 0.0)
          (setq res-start (draw-wale-subsegment v-start p-splice1-upg cut-start-p1 cut-start-p2 splice1-upg-cut-p1 splice1-upg-cut-p2 u-h-s u-tf-s))
        )

        ;; 6.2 Base Wale 및 지보재 구간별(앵커/버팀보/중첩) 다중 띠장 작도
        (setq base-start-cut (if (> L-start 0.0) splice1-base-cut-p1 cut-start-p1))
        (setq base-start-cut-p2 (if (> L-start 0.0) splice1-base-cut-p2 cut-start-p2))
        (setq base-end-cut (if (> L-end 0.0) splice2-base-cut-p1 cut-end-p1))
        (setq base-end-cut-p2 (if (> L-end 0.0) splice2-base-cut-p2 cut-end-p2))

        (setq base-d-min (if (> L-start 0.0) (- L-start (/ hp-b 2.0)) 0.0))
        (setq base-d-max (if (> L-end 0.0) (- seg-length (- L-end (/ hp-b 2.0))) seg-length))

        ;; 현재 세그먼트 지보재들의 (띠장H 띠장TF 시작거리 끝거리) 수집
        (setq sup-wale-spans '())
        (foreach s-item (cdr (assoc 'SUPPORT-LIST seg-data))
          (setq s-w-vals (tsp-get-support-item-wale-vals s-item (cdr (assoc 'WALE-SPEC seg-data)) (cdr (assoc 'WALE-CUSTOM seg-data))))
          (setq s-iv (tsp-get-support-item-dist-interval s-item v-start v-angle seg-length))
          (setq d-s (max base-d-min (car s-iv)))
          (setq d-e (min base-d-max (cdr s-iv)))
          (if (> (- d-e d-s) 1.0)
            (setq sup-wale-spans (append sup-wale-spans (list (list (nth 0 s-w-vals) (nth 3 s-w-vals) d-s d-e))))
          )
        )

        ;; 지보재가 없거나 구간 분할이 없는 경우: 기존 단일 Base Wale 작도
        (if (null sup-wale-spans)
          (setq sup-wale-spans (list (list h tf base-d-min base-d-max)))
        )

        ;; 동일 규격(H, TF)의 연속/중첩 구간 병합 및 내부 경계(Splice)에서 ±(hp-b/2) 오버랩 적용
        (setq unique-specs '())
        (foreach sp sup-wale-spans
          (if (not (vl-member-if '(lambda (u) (and (= (car u) (car sp)) (= (cadr u) (cadr sp)))) unique-specs))
            (setq unique-specs (append unique-specs (list (list (car sp) (cadr sp)))))
          )
        )
        ;; 큰 규격부터 작은 규격 순으로 정렬 (중첩 구간에서도 둘 다 실선 작도)
        (setq unique-specs (vl-sort unique-specs '(lambda (a b) (> (car a) (car b)))))

        (setq res-base nil)
        (foreach u-spec unique-specs
          (setq sub-h (car u-spec) sub-tf (cadr u-spec))
          ;; 해당 규격의 구간들 추출 및 정렬/병합
          (setq raw-ivs '())
          (foreach sp sup-wale-spans
            (if (and (= (car sp) sub-h) (= (cadr sp) sub-tf))
              (setq raw-ivs (append raw-ivs (list (cons (nth 2 sp) (nth 3 sp)))))
            )
          )
          (setq raw-ivs (vl-sort raw-ivs '(lambda (a b) (< (car a) (car b)))))
          (setq merged-ivs '())
          (foreach iv raw-ivs
            (if (null merged-ivs)
              (setq merged-ivs (list iv))
              (if (<= (car iv) (+ (cdr (last merged-ivs)) 50.0))
                (setq merged-ivs (subst (cons (car (last merged-ivs)) (max (cdr (last merged-ivs)) (cdr iv))) (last merged-ivs) merged-ivs))
                (setq merged-ivs (append merged-ivs (list iv)))
              )
            )
          )
          ;; 각 병합 구간에 대해 내부 경계면(세그먼트 중간 H-Pile)일 경우 ±(hp-b/2) 오버랩 확장 및 단차 마감
          (foreach m-iv merged-ivs
            (setq iv-s (car m-iv) iv-e (cdr m-iv))
            (setq is-internal-s (> (abs (- iv-s base-d-min)) 10.0))
            (setq is-internal-e (> (abs (- iv-e base-d-max)) 10.0))
            ;; 내부 경계 H-Pile에서 플랜지 폭(± B/2)만큼 겹침(Splice Overlap) 적용
            (if is-internal-s (setq iv-s (max base-d-min (- iv-s (/ hp-b 2.0)))))
            (if is-internal-e (setq iv-e (min base-d-max (+ iv-e (/ hp-b 2.0)))))

            (setq p-sub-s (polar v-start v-angle iv-s))
            (setq p-sub-e (polar v-start v-angle iv-e))
            (setq c-s-p1 (if is-internal-s p-sub-s base-start-cut))
            (setq c-s-p2 (if is-internal-s (polar p-sub-s offset-dir 100.0) base-start-cut-p2))
            (setq c-e-p1 (if is-internal-e p-sub-e base-end-cut))
            (setq c-e-p2 (if is-internal-e (polar p-sub-e offset-dir 100.0) base-end-cut-p2))

            (setq cur-sub-res (draw-wale-subsegment p-sub-s p-sub-e c-s-p1 c-s-p2 c-e-p1 c-e-p2 sub-h sub-tf))
            (if cur-sub-res
              (progn
                (if (null res-base) (setq res-base cur-sub-res))
                ;; 내부 구간 경계부 및 코너 Splice 경계부 단차 마감(Capping)
                (if (or is-internal-s (not res-start))
                  (make-wale-lw (nth 0 cur-sub-res) (nth 1 cur-sub-res) "_띠장(wale)" 3)
                  (if res-start (make-wale-lw (nth 0 cur-sub-res) (nth 1 cur-sub-res) "_띠장(wale)" 3))
                )
                (if (or is-internal-e (not res-end))
                  (make-wale-lw (nth 2 cur-sub-res) (nth 3 cur-sub-res) "_띠장(wale)" 3)
                  (if res-end (make-wale-lw (nth 2 cur-sub-res) (nth 3 cur-sub-res) "_띠장(wale)" 3))
                )
              )
            )
          )
        )

        ;; 6.3 End Upgrade Wale (상향 띠장: 플랜지 너비의 절반만큼 더 연장)
        (if (> L-end 0.0)
          (setq res-end (draw-wale-subsegment p-splice2-upg v-end splice2-upg-cut-p1 splice2-upg-cut-p2 cut-end-p1 cut-end-p2 u-h-e u-tf-e))
        )
        ;; [STEP 7] 코너부 끝단 및 상향 띠장 절단면 마감 (Capping)
        (if res-start
          (progn
            (make-wale-lw (nth 0 res-start) (nth 1 res-start) "_띠장(wale)" 3)
            (make-wale-lw (nth 2 res-start) (nth 3 res-start) "_띠장(wale)" 3)
          )
        )
        (if res-end
          (progn
            (make-wale-lw (nth 0 res-end) (nth 1 res-end) "_띠장(wale)" 3)
            (make-wale-lw (nth 2 res-end) (nth 3 res-end) "_띠장(wale)" 3)
          )
        )
      ) ; end progn (if is-def)
    )
    (setq exp-idx (1+ exp-idx))
  )
  (if is-upgraded-any
    (princ "\n띠장 상향 완료.")
    (princ "\n띠장 작도 완료.")
  )
  (princ)
)
