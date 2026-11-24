;;; ==========================================================================
;;; 파일명: TSP_4I_Plan.lsp
;;; 구  분: 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part I)
;;; 역  할: 연속 CIP 경로 오프셋, 코너/이종접합부 한계 탐색, 겹침 분배 및 H-Pile 배치 시뮬레이션 [tsp-safe-vla-offset, tsp-update-gap, tsp-cip-calc-chain-layout]
;;; 포함 함수: tsp-safe-vla-offset, tsp-update-gap, tsp-cip-calc-chain-layout
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-safe-vla-offset : vla-offset 안전 래퍼 (실패 시 nil 반환 - 작도 중단 방지)
;;;;==========================================================================
(defun tsp-safe-vla-offset (obj d / r)
  (setq r (vl-catch-all-apply 'vla-offset (list obj d)))
  (if (vl-catch-all-error-p r) nil (vlax-safearray-get-element (vlax-variant-value r) 0))
)

;;;;==========================================================================
;;;; defun tsp-update-gap : 리스트의 특정 인덱스(idx) 요소를 val로 교체하여 반환
;;;;==========================================================================
(defun tsp-update-gap (lst idx val / i res)
  (setq i 0 res '())
  (foreach x lst (if (= i idx) (setq res (cons val res)) (setq res (cons x res))) (setq i (1+ i)))
  (reverse res)
)

;;;;==========================================================================
;;;; defun tsp-cip-calc-chain-layout : 연속 CIP 체인의 코너 선탐색, 직선 구간 겹침 분배 및 H-Pile 시뮬레이션 수행
;;;; 반환값: (list cip-layout-items hpile-flags corner-pts max-path-len full-loop)
;;;;==========================================================================
(defun tsp-cip-calc-chain-layout (vla-pl vla-path chain-pts chain-seg-indices is-full-chain is-closed max-segs boundary-orient d wall-dist cip-ctc hpile-vals h b interval seg-idx /
                                  corner-dists corner-info-list corner-pts num-chain pl-total-len path-max-len k
                                  v-curr v-prev v-next cp-pl d-pl d-prev-pl p-prev-pl d-next-pl p-next-pl
                                  corner-geom-dir hpile-earth-dir n1 n2 turn-cross is-concave p1-s p1-e p2-s p2-e p-corner-check
                                  exact-corner-pt pt-on-path d-on-path is-mixed-hpile-corner B-val first-seg-idx prev-seg-idx end-seg-idx
                                  max-path-len full-loop cip-layout-items corner-earth-dir target-k is-mixed-c m-seg-idx pt
                                  calc-corner-dists d-idx d-A d-B L-seg search-d-A H-val tf-raw tf-val
                                  min-hpile-flange min-spiral-cage min-straight min-mixed-corner search-d-B start-corner-info end-corner-info
                                  calc-corner-limit is-mixed-start-seg is-mixed-end-seg min-corner-start min-corner-end
                                  N-int need-recalc gaps m cur-gap total-gaps overlap rem-excess max-r1 max-r2 d1 d2 req
                                  cand-indices step-i idx-left idx-right max-rc dc free-cnt avg-deduct new-gaps
                                  cur-d mid-i wrapped-d pt-on-orig orig-param orig-deriv straight-earth-dir
                                  has-mixed-start has-mixed-end hpile-flags sim-i sim-prev-target-index sim-local-i sim-last-hpile-i sim-cur-interval
                                  is-corner target-index sim-seg-data sim-interval-idx sim-gap total-cip-items)

  ;; [1. 코너 선탐색 및 강제 삽입점 산출 (Arc 정점 및 중간 절점 국소 접선 완벽 대응)]
  (setq corner-dists '() corner-info-list '() corner-pts '())
  (setq num-chain (length chain-pts))
  (setq pl-total-len (if vla-pl (vlax-curve-getDistAtParam vla-pl (vlax-curve-getEndParam vla-pl)) 0.0))
  (setq path-max-len (vlax-curve-getDistAtParam vla-path (vlax-curve-getEndParam vla-path)))
  (setq k 0)
  (while (< k num-chain)
    (setq v-curr (nth k chain-pts))
    (if is-full-chain
      (progn
        (setq v-prev (if (> k 0) (nth (1- k) chain-pts) (last chain-pts)))
        (setq v-next (if (< k (1- num-chain)) (nth (1+ k) chain-pts) (car chain-pts)))
      )
      (progn
        (setq v-prev (if (> k 0) (nth (1- k) chain-pts) nil))
        (setq v-next (if (< k (1- num-chain)) (nth (1+ k) chain-pts) nil))
      )
    )
    ;; [패치] 세그먼트 내부에 자동삭제된 중간 꺾임점이 있는 경우, 현(chain-pts)이 아닌
    ;; 실제 경계선 폴리선(vla-pl)의 코너 전후 10mm 지점을 사용하여 국소 접선/법선 오차(4.21mm 등)를 원천 차단합니다.
    (if (and vla-pl (> pl-total-len 1.0))
      (progn
        (setq cp-pl (vlax-curve-getClosestPointTo vla-pl v-curr))
        (setq d-pl (if cp-pl (vlax-curve-getDistAtPoint vla-pl cp-pl) nil))
        (if d-pl
          (progn
            (if v-prev
              (progn
                (setq d-prev-pl (- d-pl 10.0))
                (if is-full-chain
                  (while (< d-prev-pl 0.0) (setq d-prev-pl (+ d-prev-pl pl-total-len)))
                  (setq d-prev-pl (max 0.0 d-prev-pl))
                )
                (setq p-prev-pl (vlax-curve-getPointAtDist vla-pl d-prev-pl))
                (if (and p-prev-pl (> (distance p-prev-pl v-curr) 0.1))
                  (setq v-prev (list (car p-prev-pl) (cadr p-prev-pl) 0.0))
                )
              )
            )
            (if v-next
              (progn
                (setq d-next-pl (+ d-pl 10.0))
                (if is-full-chain
                  (while (>= d-next-pl pl-total-len) (setq d-next-pl (- d-next-pl pl-total-len)))
                  (setq d-next-pl (min pl-total-len d-next-pl))
                )
                (setq p-next-pl (vlax-curve-getPointAtDist vla-pl d-next-pl))
                (if (and p-next-pl (> (distance v-curr p-next-pl) 0.1))
                  (setq v-next (list (car p-next-pl) (cadr p-next-pl) 0.0))
                )
              )
            )
          )
        )
      )
    )

    (setq corner-geom-dir nil)
    (setq hpile-earth-dir nil)

    (if (and v-prev v-next)
      (progn
        (setq n1 (+ (angle v-prev v-curr) (* boundary-orient (/ pi 2.0)) pi))
        (setq n2 (+ (angle v-curr v-next) (* boundary-orient (/ pi 2.0)) pi))

        ;; 벡터 외적(Cross Product)을 통한 오목/볼록 자동 판별
        (setq turn-cross (* (sin (- (angle v-curr v-next) (angle v-prev v-curr))) boundary-orient))
        (setq is-concave (if (< turn-cross -1e-5) T nil))

        (setq p1-s (polar v-prev n1 wall-dist)) (setq p1-e (polar v-curr n1 wall-dist))
        (setq p2-s (polar v-curr n2 wall-dist)) (setq p2-e (polar v-next n2 wall-dist))
        (setq p-corner-check (inters p1-s p1-e p2-s p2-e nil))
        (if (null p-corner-check) (setq p-corner-check (polar v-curr n1 wall-dist)))

        ;; CIP 원 생성을 위한 기하학적 중심 방향
        (setq corner-geom-dir (angle v-curr p-corner-check))
        (if (< (distance v-curr p-corner-check) 0.1) (setq corner-geom-dir n1))

        ;; 오목 모서리(배면이 좁아지는 구간)일 경우 H-pile을 이등분선 대신 다음 세그먼트 직교각(n2)에 맞춤
        (if is-concave
          (setq hpile-earth-dir n2)
          (setq hpile-earth-dir corner-geom-dir)
        )
      )
      (progn
        (if v-next
          (setq corner-geom-dir (+ (angle v-curr v-next) (* boundary-orient (/ pi 2.0)) pi))
          (setq corner-geom-dir (+ (angle v-prev v-curr) (* boundary-orient (/ pi 2.0)) pi))
        )
        (setq hpile-earth-dir corner-geom-dir)
      )
    )

    ;; vla-path(오프셋 경로) 상의 정확한 모서리/라운드 정점 위치 거리 도출
    (setq exact-corner-pt (polar v-curr corner-geom-dir wall-dist))
    (setq pt-on-path (vlax-curve-getClosestPointTo vla-path exact-corner-pt))
    (if pt-on-path
      (progn
        (setq d-on-path (vlax-curve-getDistAtPoint vla-path pt-on-path))
        (if d-on-path
          (progn
            ;; [패치] CIP 체인의 시작점(k=0) 또는 끝점(k=num-chain-1)이 H-Pile+토류판 세그먼트와
            ;; 만나는 이종 벽체 접합 모서리인지 판별합니다.
            (setq is-mixed-hpile-corner nil)
            (if (not is-full-chain)
              (progn
                (setq B-val (if (= (type b) 'STR) (atof b) b))
                (if (= k 0)
                  (progn
                    (setq first-seg-idx (car chain-seg-indices))
                    (setq prev-seg-idx (if is-closed (if (= first-seg-idx 0) (1- max-segs) (1- first-seg-idx)) (1- first-seg-idx)))
                    (if (and (>= prev-seg-idx 0)
                             (cdr (assoc 'IS-DEFINED (nth prev-seg-idx *segment-list*)))
                             (/= (cdr (assoc 'WALL-TYPE (nth prev-seg-idx *segment-list*))) "CIP"))
                      (setq is-mixed-hpile-corner T)
                      (setq d-on-path (+ d-on-path (/ B-val 2.0)))
                    )
                  )
                )
                (if (= k (1- num-chain))
                  (progn
                    (setq end-seg-idx (last chain-seg-indices))
                    (if (and (< end-seg-idx max-segs)
                             (cdr (assoc 'IS-DEFINED (nth end-seg-idx *segment-list*)))
                             (/= (cdr (assoc 'WALL-TYPE (nth end-seg-idx *segment-list*))) "CIP"))
                      (setq is-mixed-hpile-corner T)
                      (setq d-on-path (- d-on-path (/ B-val 2.0)))
                    )
                  )
                )
                ;; 변경된 거리로 삽입점 좌표 갱신 (추후 간섭 체크 등을 위해 정확한 위치 보존)
                (setq pt-on-path (vlax-curve-getPointAtDist vla-path d-on-path))
              )
            )
            (setq corner-dists (append corner-dists (list d-on-path)))

            ;; H-pile 배치 기준 방향(hpile-earth-dir) 및 이종 벽체 모서리 플래그(is-mixed-hpile-corner)를 리스트에 저장
            (setq corner-info-list (append corner-info-list (list (list d-on-path hpile-earth-dir k is-mixed-hpile-corner))))
          )
        )
        (setq corner-pts (append corner-pts (list (list pt-on-path))))
      )
      (setq corner-pts (append corner-pts (list (list exact-corner-pt))))
    )
    (setq k (1+ k))
  )

  (setq corner-dists (vl-sort corner-dists '<))
  (setq max-path-len (vlax-curve-getdistatparam vla-path (vlax-curve-getendparam vla-path)))
  (setq full-loop is-full-chain)

  ;; [2. 직선구간 겹침(Overlap) 양끝 공유 분배 알고리즘]
  (setq cip-layout-items '())

  ;; 코너 선배치 (단, H-Pile+토류판과 만나는 이종 모서리는 4C에서 단독 H-Pile을 모서리에 그리므로
  ;; CIP 모듈에서는 모서리 CIP/H-Pile을 생략하고 세그먼트 카운트만 +1 보정함)
  (foreach c-info corner-info-list
    (setq d-on-path (car c-info))
    (setq corner-earth-dir (cadr c-info))
    (setq target-k (caddr c-info))
    (setq is-mixed-c (nth 3 c-info))
    (if (not full-loop)
      (if (>= target-k (1- num-chain)) (setq target-k (- num-chain 2)))
    )
    (if is-mixed-c
      ;; 시작점(k=0)이 H-Pile 세그먼트와 만나는 경우, draw-corner-numbers의 CIP 시작점 -1 차감과
      ;; 정합성을 맞추기 위해 해당 CIP 세그먼트 카운트를 +1 보상합니다.
      (if (= (caddr c-info) 0)
        (progn
          (setq m-seg-idx (nth target-k chain-seg-indices))
          (if m-seg-idx
            (setq *tsp-segment-pile-counts*
              (update-list-item *tsp-segment-pile-counts* m-seg-idx
                (1+ (if (nth m-seg-idx *tsp-segment-pile-counts*) (nth m-seg-idx *tsp-segment-pile-counts*) 0))))
          )
        )
      )
      (progn
        (setq pt (vlax-curve-getPointAtDist vla-path d-on-path))
        (if pt
          (setq cip-layout-items (append cip-layout-items (list (list pt T corner-earth-dir target-k d-on-path))))
        )
      )
    )
  )

  ;; 폐합선일 경우, 1번 세그먼트(마지막 코너->첫 코너) 누락을 막기 위해 가상의 순환 거리를 삽입
  (setq calc-corner-dists corner-dists)
  (if full-loop
    (setq calc-corner-dists (append calc-corner-dists (list (+ (car corner-dists) max-path-len))))
  )

  ;; 직선 구간 분배
  (setq d-idx 0)
  (while (< d-idx (1- (length calc-corner-dists)))
    (setq d-A (nth d-idx calc-corner-dists))
    (setq d-B (nth (1+ d-idx) calc-corner-dists))
    (setq L-seg (- d-B d-A))

    (setq target-k 0)
    (setq search-d-A (if (>= d-A max-path-len) (- d-A max-path-len) d-A))
    (foreach c-info corner-info-list
      (if (equal (car c-info) search-d-A 0.01) (setq target-k (caddr c-info)))
    )
    (if (not full-loop)
      (if (>= target-k (1- num-chain)) (setq target-k (- num-chain 2)))
    )

    (if (> L-seg 0.01)
      (progn
        ;; [1] 동적 한계 피치 계산을 위한 H-Pile 제원 파싱
        (setq H-val (if (= (type h) 'STR) (atof h) h))
        (setq B-val (if (= (type b) 'STR) (atof b) b))
        (setq tf-raw (nth 3 hpile-vals))
        (setq tf-val (if (= (type tf-raw) 'STR) (atof tf-raw) tf-raw))

        ;; [2] 직선부 최소 피치 방정식 해 (H-Pile 플랜지 간섭 한계 + 나선철근 0.1D 옵셋 한계 동시 반영)
        (setq min-hpile-flange (+ (/ B-val 2.0) (sqrt (abs (- (* wall-dist wall-dist) (expt (- (/ H-val 2.0) tf-val) 2.0))))))
        (setq min-spiral-cage (max (* 0.1 d) (- cip-ctc (* 0.1 d))))
        (setq min-straight (max min-hpile-flange min-spiral-cage))

        ;; [2-1] 이종 벽체(H-Pile+토류판 <-> CIP) 모서리의 단독 H-Pile(경계선 기준 배치)과
        ;;       그 옆 첫 번째 비어있는 CIP 원이 겹치지 않는 외접 한계 피치
        (setq min-mixed-corner (+ (/ B-val 2.0) (sqrt (max 0.0 (- (* wall-dist wall-dist) (expt (- H-val wall-dist tf-val) 2.0))))))

        ;; [3] 코너부 최소 피치 계산을 위한 접점 탐색기
        (setq search-d-B (if (>= d-B max-path-len) (- d-B max-path-len) d-B))
        (setq start-corner-info nil end-corner-info nil)
        (foreach c-info corner-info-list
          (if (equal (car c-info) search-d-A 0.01) (setq start-corner-info c-info))
          (if (equal (car c-info) search-d-B 0.01) (setq end-corner-info c-info))
        )

        ;; 모서리부 H-Pile 간섭 회피를 위한 실제 곡선(Arc) 경로 탐색 알고리즘
        (defun calc-corner-limit (c-info is-start / c-earth c-inward c-perp c-pt c1x c1y c2x c2y target-k-val d-base search-dir low high mid iter test-d wrap-d pt-test dist1 dist2)
          (if c-info
            (progn
              (setq target-k-val (caddr c-info))
              (if (and (not is-full-chain) (or (= target-k-val 0) (= target-k-val (1- num-chain))))
                (if (nth 3 c-info) min-mixed-corner min-straight)
                (progn
                  (setq c-earth (cadr c-info) c-inward (+ c-earth pi) c-perp (+ c-inward (/ pi 2.0)))

                  ;; 해당 코너 CIP의 실제 중심점 추출
                  (setq c-pt (vlax-curve-getPointAtDist vla-path (car c-info)))

                  ;; H-Pile 아래 플랜지 바닥면 양쪽 꼭지점의 절대 좌표 계산
                  (setq c1x (+ (car c-pt) (* (/ H-val 2.0) (cos c-inward)) (* (/ B-val 2.0) (cos c-perp))))
                  (setq c1y (+ (cadr c-pt) (* (/ H-val 2.0) (sin c-inward)) (* (/ B-val 2.0) (sin c-perp))))
                  (setq c2x (+ (car c-pt) (* (/ H-val 2.0) (cos c-inward)) (* (- (/ B-val 2.0)) (cos c-perp))))
                  (setq c2y (+ (cadr c-pt) (* (/ H-val 2.0) (sin c-inward)) (* (- (/ B-val 2.0)) (sin c-perp))))

                  ;; 이분법(Bisection)을 통해 vla-path 곡선을 따라가며 간섭 없는 최소 경로 거리 탐색
                  (setq d-base (car c-info))
                  (setq search-dir (if is-start 1.0 -1.0))
                  (setq low 0.0 high 1500.0)
                  (setq iter 0)
                  (while (< iter 50)
                    (setq mid (/ (+ low high) 2.0))
                    (setq test-d (+ d-base (* search-dir mid)))

                    ;; 폐합선 보정
                    (setq wrap-d test-d)
                    (if (and full-loop (>= wrap-d max-path-len)) (setq wrap-d (- wrap-d max-path-len)))
                    (if (and full-loop (< wrap-d 0.0)) (setq wrap-d (+ wrap-d max-path-len)))
                    (if (> wrap-d max-path-len) (setq wrap-d max-path-len))
                    (if (< wrap-d 0.0) (setq wrap-d 0.0))

                    (setq pt-test (vlax-curve-getPointAtDist vla-path wrap-d))
                    (if pt-test
                      (progn
                        (setq dist1 (distance pt-test (list c1x c1y 0.0)))
                        (setq dist2 (distance pt-test (list c2x c2y 0.0)))
                        (if (< (min dist1 dist2) wall-dist)
                          (setq low mid)
                          (setq high mid)
                        )
                      )
                      (setq high mid)
                    )
                    (setq iter (1+ iter))
                  )
                  (max high min-straight)
                )
              )
            )
            min-straight
          )
        )

        (setq is-mixed-start-seg (and start-corner-info (nth 3 start-corner-info)))
        (setq is-mixed-end-seg (and end-corner-info (nth 3 end-corner-info)))
        (setq min-corner-start (calc-corner-limit start-corner-info T))
        (setq min-corner-end (calc-corner-limit end-corner-info nil))

        ;; [4] 겹침량 연쇄 분배 (Cascading Overlap)
        (setq N-int (fix (+ (/ L-seg cip-ctc) 0.9999)))
        (if (< N-int 1) (setq N-int 1))

        (setq need-recalc T)
        (while need-recalc
          (setq gaps '() m 0)
          (while (< m N-int)
            (setq cur-gap cip-ctc)
            (if (= m 0)
              (setq cur-gap (if is-mixed-start-seg min-corner-start (max cip-ctc min-corner-start)))
            )
            (if (= m (1- N-int))
              (setq cur-gap (if is-mixed-end-seg min-corner-end (max cur-gap min-corner-end)))
            )
            (setq gaps (append gaps (list cur-gap)))
            (setq m (1+ m))
          )
          (setq total-gaps 0.0)
          (foreach g gaps (setq total-gaps (+ total-gaps g)))
          (setq overlap (- total-gaps L-seg))

          (cond
            ((< overlap -0.001)
             (setq N-int (1+ N-int))
            )
            ((and (> N-int 1) (>= overlap (- cip-ctc 0.001)))
             (setq N-int (1- N-int))
            )
            (t
             (setq need-recalc nil)
            )
          )
        )

        (setq rem-excess overlap)

        ;; [단계별 겹침 분배]
        (if (> rem-excess 0.001)
          (progn
            ;; 1단계: 기본 양끝 2개 갭에서 최우선 균등/대칭 차감
            (setq max-r1 (max 0.0 (- (nth 0 gaps) min-corner-start)))
            (setq max-r2 (max 0.0 (- (nth (1- N-int) gaps) min-corner-end)))
            (if (= 0 (1- N-int))
              (setq d1 (min rem-excess max-r1) d2 0.0)
              (progn
                (setq req (/ rem-excess 2.0) d1 (min req max-r1) d2 (min req max-r2))
                (if (< d1 req) (setq d2 (min (- rem-excess d1) max-r2)))
                (if (< d2 req) (setq d1 (min (- rem-excess d2) max-r1)))
              )
            )
            (setq gaps (tsp-update-gap gaps 0 (- (nth 0 gaps) d1)))
            (if (/= 0 (1- N-int)) (setq gaps (tsp-update-gap gaps (1- N-int) (- (nth (1- N-int) gaps) d2))))
            (setq rem-excess (- rem-excess d1 d2))

            ;; 2단계: 안쪽 갭을 1개씩 순차 동원하여 겹침 분배
            (if (and (> rem-excess 0.001) (>= N-int 3))
              (progn
                (setq cand-indices '() step-i 1)
                (while (<= step-i (- N-int 1 step-i))
                  (setq idx-left step-i)
                  (setq idx-right (- N-int 1 step-i))
                  (setq cand-indices (append cand-indices (list idx-left)))
                  (if (/= idx-left idx-right)
                    (setq cand-indices (append cand-indices (list idx-right)))
                  )
                  (setq step-i (1+ step-i))
                )

                (foreach c-idx cand-indices
                  (if (> rem-excess 0.001)
                    (progn
                      (setq max-rc (max 0.0 (- (nth c-idx gaps) min-straight)))
                      (setq dc (min rem-excess max-rc))
                      (setq gaps (tsp-update-gap gaps c-idx (- (nth c-idx gaps) dc)))
                      (setq rem-excess (- rem-excess dc))
                    )
                  )
                )
              )
            )

            ;; 3단계: 극한 예외 상황 폴백 (이종 모서리 인접 양끝 갭은 외접 거리 보존)
            (if (> rem-excess 0.001)
              (progn
                (setq free-cnt 0 m 0)
                (while (< m N-int)
                  (if (not (or (and (= m 0) is-mixed-start-seg) (and (= m (1- N-int)) is-mixed-end-seg)))
                    (setq free-cnt (1+ free-cnt))
                  )
                  (setq m (1+ m))
                )
                (if (> free-cnt 0)
                  (progn
                    (setq avg-deduct (/ rem-excess free-cnt) m 0 new-gaps '())
                    (while (< m N-int)
                      (if (or (and (= m 0) is-mixed-start-seg) (and (= m (1- N-int)) is-mixed-end-seg))
                        (setq new-gaps (append new-gaps (list (nth m gaps))))
                        (setq new-gaps (append new-gaps (list (- (nth m gaps) avg-deduct))))
                      )
                      (setq m (1+ m))
                    )
                    (setq gaps new-gaps)
                  )
                )
              )
            )
          )
        )

        ;; [5] 최종 산출된 Gap을 바탕으로 좌표 배열 실행
        (setq cur-d d-A mid-i 0)
        (while (< mid-i (1- N-int))
          (setq cur-d (+ cur-d (nth mid-i gaps)))
          (setq wrapped-d cur-d)
          (if (and full-loop (>= wrapped-d max-path-len)) (setq wrapped-d (- wrapped-d max-path-len)))
          (if (> wrapped-d max-path-len) (setq wrapped-d max-path-len))

          (setq pt (vlax-curve-getPointAtDist vla-path wrapped-d))
          (if pt
            (progn
              (setq pt-on-orig (vlax-curve-getClosestPointTo vla-pl pt))
              (if pt-on-orig
                (progn
                  (setq orig-param (vlax-curve-getParamAtPoint vla-pl pt-on-orig))
                  (if (null orig-param) (setq orig-param (+ target-k 0.5)))
                  (setq orig-deriv (vlax-curve-getFirstDeriv vla-pl orig-param))
                  (setq straight-earth-dir (+ (angle '(0 0 0) orig-deriv) (* boundary-orient (/ pi 2.0)) pi))
                  (setq cip-layout-items (append cip-layout-items (list (list pt nil straight-earth-dir target-k cur-d))))
                )
              )
            )
          )
          (setq mid-i (1+ mid-i))
        )
      )
    )
    (setq d-idx (1+ d-idx))
  )

  (setq cip-layout-items (vl-sort cip-layout-items '(lambda (a b) (< (nth 4 a) (nth 4 b)))))

  ;; [2.5] H-Pile+토류판 세그먼트와 접하는 양끝단 CIP 공의 H-Pile 1칸 이동(Shift) 사전 시뮬레이션
  (setq has-mixed-start nil has-mixed-end nil)
  (foreach c-info corner-info-list
    (if (and (= (caddr c-info) 0) (nth 3 c-info))
      (setq has-mixed-start T)
    )
    (if (and (= (caddr c-info) (1- num-chain)) (nth 3 c-info))
      (setq has-mixed-end T)
    )
  )

  ;; 1차 패스: 기존 주기 규칙에 따른 H-Pile 배치 여부(hpile-flags) 사전 산출
  (setq hpile-flags '())
  (setq sim-i 0 sim-prev-target-index -1 sim-local-i 0 sim-last-hpile-i -999 sim-cur-interval interval)
  (foreach cip-item cip-layout-items
    (setq is-corner (nth 1 cip-item))
    (setq target-k (nth 3 cip-item))
    (setq target-index (nth target-k chain-seg-indices))
    (if (null target-index) (setq target-index seg-idx))

    (if (/= target-index sim-prev-target-index)
      (progn
        (setq sim-seg-data (nth target-index *segment-list*))
        (setq sim-interval-idx (if (assoc 'CIP-INTERVAL-IDX sim-seg-data) (cdr (assoc 'CIP-INTERVAL-IDX sim-seg-data)) *tsp-cip-interval-idx*))
        (setq sim-cur-interval (cond ((= sim-interval-idx "1") 2) ((= sim-interval-idx "2") 3) (t 1)))
        (if is-corner
          (setq sim-local-i 0)
          (if (/= sim-prev-target-index -1)
            (progn
              (setq sim-local-i 0)
              (setq sim-gap (- sim-i sim-last-hpile-i))
              (if (< sim-gap sim-cur-interval)
                (setq sim-local-i sim-gap)
              )
            )
            (setq sim-local-i 0)
          )
        )
        (setq sim-prev-target-index target-index)
      )
    )
    (if (and is-corner (= target-index sim-prev-target-index))
      (setq sim-local-i 0)
    )

    (if (or is-corner (= (rem sim-local-i sim-cur-interval) 0))
      (progn
        (setq hpile-flags (append hpile-flags (list T)))
        (setq sim-last-hpile-i sim-i)
      )
      (setq hpile-flags (append hpile-flags (list nil)))
    )
    (setq sim-local-i (1+ sim-local-i))
    (setq sim-i (1+ sim-i))
  )

  ;; 2차 패스: 시작단(0번 공) 및 끝단(마지막 공)이 H-Pile+토류판 세그먼트와 접할 때 H-Pile을 안쪽 빈 공으로 1칸 이동
  (setq total-cip-items (length hpile-flags))
  (if (and has-mixed-start (>= total-cip-items 1) (nth 0 hpile-flags))
    (progn
      ;; 첫 번째 CIP 공(0번)은 빈 공으로 변경
      (setq hpile-flags (tsp-update-gap hpile-flags 0 nil))
      ;; 바로 옆 두 번째 CIP 공(1번)이 코너가 아니고 비어 있으면 H-Pile 배치 (이미 있으면 생략)
      (if (and (>= total-cip-items 2)
               (not (nth 1 (nth 1 cip-layout-items)))
               (not (nth 1 hpile-flags)))
        (setq hpile-flags (tsp-update-gap hpile-flags 1 T))
      )
    )
  )
  (if (and has-mixed-end (>= total-cip-items 1) (nth (1- total-cip-items) hpile-flags))
    (progn
      ;; 마지막 CIP 공(N-1번)은 빈 공으로 변경
      (setq hpile-flags (tsp-update-gap hpile-flags (1- total-cip-items) nil))
      ;; 바로 앞 CIP 공(N-2번)이 코너가 아니고 비어 있으면 H-Pile 배치 (단, 0번 공이 시작단 이종 접합부인 경우는 제외)
      (if (and (>= total-cip-items 2)
               (not (and (= (- total-cip-items 2) 0) has-mixed-start))
               (not (nth 1 (nth (- total-cip-items 2) cip-layout-items)))
               (not (nth (- total-cip-items 2) hpile-flags)))
        (setq hpile-flags (tsp-update-gap hpile-flags (- total-cip-items 2) T))
      )
    )
  )

  (list cip-layout-items hpile-flags corner-pts max-path-len full-loop)
)
