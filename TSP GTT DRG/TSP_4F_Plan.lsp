;;; ==========================================================================
;;; 파일명: TSP_4F_Plan.lsp
;;; 구  분: 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part F)
;;; 역  할: 평면 띠장 오프셋/상향, H-Pile/토류판 배치, 사보강재/화타쐐기, 차수그라우팅, 연속 CIP 평면 배열, 세그먼트/모서리 번호표 및 수동말뚝 삭제 [tsp-draw-hpile-grouting-per-segment, tsp-boundary-path-info, tsp-dist-of-near-vertex, tsp-expand-segments-by-geometry]
;;; 포함 함수: tsp-draw-hpile-grouting-per-segment, tsp-boundary-path-info, tsp-dist-of-near-vertex, tsp-expand-segments-by-geometry
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-safe-vla-offset : vla-offset 안전 래퍼 (실패 시 nil 반환 - 작도 중단 방지)
(defun tsp-safe-vla-offset (obj d / r)
  (setq r (vl-catch-all-apply 'vla-offset (list obj d)))
  (if (vl-catch-all-error-p r) nil (vlax-safearray-get-element (vlax-variant-value r) 0))
)

;;;; defun tsp-draw-hpile-grouting-per-segment : H-Pile+토류판 세그먼트별 배면 차수그라우팅 평면 작도
;;;;==========================================================================
(defun tsp-draw-hpile-grouting-per-segment (boundary-ent boundary-orient / is-closed orig-vertices n-verts max-segs visited-segs seg-idx seg-data seg-wall-type seg-grout-idx chain-pts chain-seg-indices j stop-chain next-data next-type next-grout-idx end-pt is-full-chain temp-pl vla-pl current-spec hpile-vals h cur-grouting-dia-idx grouting-dia cur-grouting-offset-idx cur-grouting-offset-custom default-hpile-grout-offset actual-grout-offset old-offsetgap vla-grout-offset-list vla-grout-path grout-total-len grout-sample-inside-count ratio grout-sample-pt grout-sample-fail-count pt-off pt-orig orig-param-test param-safe orig-deriv orig-tangent expected-offset-norm vec-expect-x vec-expect-y vec-actual-x vec-actual-y dot-prod max-grout-len grout-dist rem-grout-len drawn-grout-pts grout-pt p-on-orig p-param target-k-grout target-index-grout cur-seg-data cur-grouting-space-idx cur-grouting-custom cur-grouting-dia grout-space-val skip-grout tmp-off)
  (setq is-closed (is-closed-polyline boundary-ent))
  (setq orig-vertices (mapcar '(lambda (s) (cdr (assoc 'V-START s))) *segment-list*))
  (if (not is-closed)
    (setq orig-vertices (append orig-vertices (list (cdr (assoc 'V-END (last *segment-list*))))))
  )
  (setq n-verts (length orig-vertices))
  (setq max-segs (if is-closed n-verts (1- n-verts)))
  (setq visited-segs '())
  (setq seg-idx 0)

  (while (< seg-idx max-segs)
    (setq seg-data (nth seg-idx *segment-list*))
    (setq seg-wall-type (cdr (assoc 'WALL-TYPE seg-data)))
    (if (not seg-wall-type) (setq seg-wall-type "HPILE"))
    (setq seg-grout-idx (if (assoc 'CIP-GROUTING-IDX seg-data) (cdr (assoc 'CIP-GROUTING-IDX seg-data)) "0"))

    (if (and (= seg-wall-type "HPILE")
             (cdr (assoc 'IS-DEFINED seg-data))
             (/= seg-grout-idx "2")
             (not (member seg-idx visited-segs)))
      (progn
        ;; 1. 연속된 H-Pile+토류판(그라우팅 활성) 세그먼트 체인 구성
        (setq chain-pts (list (nth seg-idx orig-vertices)))
        (setq chain-seg-indices (list seg-idx))
        (setq visited-segs (cons seg-idx visited-segs))
        (setq j (if is-closed (rem (1+ seg-idx) max-segs) (1+ seg-idx)))
        (setq stop-chain nil)

        (while (and (not stop-chain) (/= j seg-idx) (if is-closed T (< j max-segs)) (not (member j visited-segs)))
          (setq next-data (nth j *segment-list*))
          (setq next-type (cdr (assoc 'WALL-TYPE next-data)))
          (if (not next-type) (setq next-type "HPILE"))
          (setq next-grout-idx (if (assoc 'CIP-GROUTING-IDX next-data) (cdr (assoc 'CIP-GROUTING-IDX next-data)) "0"))
          (if (and (= next-type "HPILE") (cdr (assoc 'IS-DEFINED next-data)) (/= next-grout-idx "2"))
            (progn
              (setq chain-pts (append chain-pts (list (nth j orig-vertices))))
              (setq chain-seg-indices (append chain-seg-indices (list j)))
              (setq visited-segs (cons j visited-segs))
              (setq j (if is-closed (rem (1+ j) max-segs) (1+ j)))
            )
            (setq stop-chain T)
          )
        )
        (setq end-pt (if (< j n-verts) (nth j orig-vertices) (nth 0 orig-vertices)))
        (setq chain-pts (append chain-pts (list end-pt)))
        (setq chain-seg-indices (append chain-seg-indices (list j)))

        ;; 2. 기준 세그먼트의 H-Pile 높이(H) 및 그라우팅 기준선 오프셋 산출
        (setq current-spec (cdr (assoc 'WALL-SPEC seg-data)))
        (if (= current-spec "User-defined")
          (setq hpile-vals (cdr (assoc 'WALL-CUSTOM seg-data)))
          (setq hpile-vals (parse-h-spec current-spec))
        )
        (if (null hpile-vals) (setq hpile-vals '(300 300 10 15)))
        (setq h (float (nth 0 hpile-vals)))

        (setq cur-grouting-dia-idx (if (assoc 'CIP-GROUTING-IDX seg-data) (cdr (assoc 'CIP-GROUTING-IDX seg-data)) "0"))
        (setq grouting-dia (if (= cur-grouting-dia-idx "1") 600.0 200.0))
        (setq cur-grouting-offset-idx (if (assoc 'CIP-GROUTING-OFFSET-IDX seg-data) (cdr (assoc 'CIP-GROUTING-OFFSET-IDX seg-data)) "0"))
        (setq cur-grouting-offset-custom (if (assoc 'CIP-GROUTING-OFFSET-CUSTOM seg-data) (cdr (assoc 'CIP-GROUTING-OFFSET-CUSTOM seg-data)) ""))
        (setq default-hpile-grout-offset (+ h (/ grouting-dia 2.0)))
        (setq actual-grout-offset
          (cond
            ((= cur-grouting-offset-idx "1")
             (if (and cur-grouting-offset-custom (/= cur-grouting-offset-custom "")) (atof cur-grouting-offset-custom) default-hpile-grout-offset))
            ((= cur-grouting-offset-idx "2") h)
            (t default-hpile-grout-offset)
          )
        )

        ;; 3. 임시 연속 폴리선 생성 및 배면 방향 오프셋
        (setq is-full-chain (and is-closed (> (length chain-pts) 1) (< (distance (car chain-pts) (last chain-pts)) 0.001)))
        (if is-full-chain
          (setq chain-pts (reverse (cdr (reverse chain-pts))))
        )
        (setq geo-path-pts (tsp-geo-augment-chain boundary-ent chain-pts))
        (if (or (null geo-path-pts) (< (length geo-path-pts) 2)) (setq geo-path-pts chain-pts))
        (setq temp-pl (entmakex (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                                              (cons 90 (length geo-path-pts)) (cons 70 (if is-full-chain 1 0)))
                                        (mapcar '(lambda (p) (list 10 (car p) (cadr p))) geo-path-pts))))
        (setq vla-pl (if temp-pl (vl-catch-all-apply 'vlax-ename->vla-object (list temp-pl)) nil))
        (if (vl-catch-all-error-p vla-pl) (setq vla-pl nil))

        (setq old-offsetgap (getvar "OFFSETGAPTYPE"))
        (setvar "OFFSETGAPTYPE" 1)

        (setq vla-grout-offset-list (vl-catch-all-apply 'vla-offset (list vla-pl actual-grout-offset)))
        (if (not (vl-catch-all-error-p vla-grout-offset-list))
          (progn
            (setq vla-grout-path (vlax-safearray-get-element (vlax-variant-value vla-grout-offset-list) 0))

            (if is-closed
              (progn
                (setq grout-total-len (vlax-curve-getDistAtParam vla-grout-path (vlax-curve-getEndParam vla-grout-path)))
                (setq grout-sample-inside-count 0)
                (foreach ratio '(0.25 0.5 0.75)
                  (setq grout-sample-pt (vlax-curve-getPointAtDist vla-grout-path (* grout-total-len ratio)))
                  (if (and grout-sample-pt (tsp-is-point-inside grout-sample-pt orig-vertices))
                    (setq grout-sample-inside-count (1+ grout-sample-inside-count))
                  )
                )
                (if (>= grout-sample-inside-count 2)
                  (progn
                    (setq tmp-off (tsp-safe-vla-offset vla-pl (- actual-grout-offset)))
                    (if tmp-off
                      (progn
                        (princ "\n[경고] 그라우팅 오프셋 재생성 실패 - 기존 오프셋 경로로 대체합니다.")
                        (vla-delete vla-grout-path)
                        (setq vla-grout-path tmp-off)
                      )
                    )
                  )
                )
              )
              (progn
                (setq grout-total-len (vlax-curve-getDistAtParam vla-grout-path (vlax-curve-getEndParam vla-grout-path)))
                (setq grout-sample-fail-count 0)
                (foreach ratio '(0.25 0.5 0.75)
                  (setq pt-off (vlax-curve-getPointAtDist vla-grout-path (* grout-total-len ratio)))
                  (if pt-off
                    (progn
                      (setq pt-orig (vlax-curve-getClosestPointTo vla-pl pt-off))
                      (setq orig-param-test (vlax-curve-getParamAtPoint vla-pl pt-orig))
                      (setq param-safe orig-param-test)
                      (if (equal param-safe (fix param-safe) 1e-4)
                        (if (>= param-safe (vlax-curve-getEndParam vla-pl))
                          (setq param-safe (- param-safe 1e-3))
                          (setq param-safe (+ param-safe 1e-3))
                        )
                      )
                      (setq orig-deriv (vlax-curve-getFirstDeriv vla-pl param-safe))
                      (setq orig-tangent (angle '(0 0 0) orig-deriv))
                      (setq expected-offset-norm (- orig-tangent (* boundary-orient (/ pi 2.0))))
                      (setq vec-expect-x (cos expected-offset-norm))
                      (setq vec-expect-y (sin expected-offset-norm))
                      (setq vec-actual-x (- (car pt-off) (car pt-orig)))
                      (setq vec-actual-y (- (cadr pt-off) (cadr pt-orig)))
                      (setq dot-prod (+ (* vec-actual-x vec-expect-x) (* vec-actual-y vec-expect-y)))
                      (if (< dot-prod 0.0)
                        (setq grout-sample-fail-count (1+ grout-sample-fail-count))
                      )
                    )
                  )
                )
                (if (>= grout-sample-fail-count 2)
                  (progn
                    (setq tmp-off (tsp-safe-vla-offset vla-pl (- actual-grout-offset)))
                    (if tmp-off
                      (progn
                        (princ "\n[경고] 그라우팅 오프셋 재생성 실패 - 기존 오프셋 경로로 대체합니다.")
                        (vla-delete vla-grout-path)
                        (setq vla-grout-path tmp-off)
                      )
                    )
                  )
                )
              )
            )
            (setvar "OFFSETGAPTYPE" old-offsetgap)

            ;; 4. 오프셋 경로를 따라 세그먼트별 그라우팅 간격/규격으로 작도
            (setq max-grout-len (vlax-curve-getdistatparam vla-grout-path (vlax-curve-getendparam vla-grout-path)))
            (setq grout-dist 0.0)
            (create-layer-if-not-exists "_그라우팅" "5")
            (setq drawn-grout-pts '())

            (while (<= grout-dist max-grout-len)
              (setq grout-pt (vlax-curve-getpointatdist vla-grout-path grout-dist))
              (setq p-on-orig (vlax-curve-getClosestPointTo vla-pl grout-pt))
              (setq p-param (vlax-curve-getParamAtPoint vla-pl p-on-orig))
              (setq target-k-grout (fix p-param))
              (if (>= target-k-grout (1- (length chain-pts))) (setq target-k-grout (- (length chain-pts) 2)))
              (setq target-index-grout (nth target-k-grout chain-seg-indices))
              (if (null target-index-grout) (setq target-index-grout seg-idx))
              (setq cur-seg-data (nth target-index-grout *segment-list*))

              (setq cur-grouting-space-idx (if (assoc 'CIP-GROUTING-SPACE-IDX cur-seg-data) (cdr (assoc 'CIP-GROUTING-SPACE-IDX cur-seg-data)) "0"))
              (setq cur-grouting-custom (if (assoc 'CIP-GROUTING-SPACE-CUSTOM cur-seg-data) (cdr (assoc 'CIP-GROUTING-SPACE-CUSTOM cur-seg-data)) ""))
              (setq cur-grouting-dia-idx (if (assoc 'CIP-GROUTING-IDX cur-seg-data) (cdr (assoc 'CIP-GROUTING-IDX cur-seg-data)) "0"))
              (setq cur-grouting-dia (if (= cur-grouting-dia-idx "1") 600.0 200.0))

              (setq grout-space-val
                (cond
                  ((= cur-grouting-space-idx "0") 200.0)
                  ((= cur-grouting-space-idx "1") 400.0)
                  ((= cur-grouting-space-idx "2") 600.0)
                  (t (if (and cur-grouting-custom (/= cur-grouting-custom "")) (atof cur-grouting-custom) 200.0))
                )
              )

              (setq skip-grout (= cur-grouting-dia-idx "2"))
              ;; 시작점과 사실상 동일 위치(0.15배 미만)일 때만 중복 스킵
              (if (and (not skip-grout) is-full-chain drawn-grout-pts)
                (if (< (distance grout-pt (last drawn-grout-pts)) (* grout-space-val 0.15))
                  (setq skip-grout T)
                )
              )

              (if (not skip-grout)
                (progn
                  (tsp-draw-advanced-grouting grout-pt cur-grouting-dia '() 0.0)
                  (setq drawn-grout-pts (cons grout-pt drawn-grout-pts))
                )
              )

              ;; 종단 이음매(1번 모서리 등) 마지막 2칸 구간 균등 2등분 배분
              (setq rem-grout-len (- max-grout-len grout-dist))
              (if is-full-chain
                (cond
                  ((<= rem-grout-len (* grout-space-val 1.05))
                   (setq grout-dist (+ max-grout-len 10.0)))
                  ((<= rem-grout-len (* grout-space-val 2.0))
                   (setq grout-dist (+ grout-dist (/ rem-grout-len 2.0))))
                  (t
                   (setq grout-dist (+ grout-dist grout-space-val)))
                )
                (cond
                  ((<= rem-grout-len 1.0)
                   (setq grout-dist (+ max-grout-len 10.0)))
                  ((<= rem-grout-len (* grout-space-val 1.05))
                   (setq grout-dist max-grout-len))
                  ((<= rem-grout-len (* grout-space-val 2.0))
                   (setq grout-dist (+ grout-dist (/ rem-grout-len 2.0))))
                  (t
                   (setq grout-dist (+ grout-dist grout-space-val)))
                )
              )
            )
            (vla-delete vla-grout-path)
          )
          (setvar "OFFSETGAPTYPE" old-offsetgap)
        )
        (if temp-pl (entdel temp-pl))
      )
    )
    (setq seg-idx (1+ seg-idx))
  )
)

;;;;==========================================================================
;;;;==========================================================================
;;;; defun tsp-boundary-path-info : 경계선 폴리라인의 원본 정점 / 누적거리 / 총길이 / 폐합여부 반환
;;;;   반환: (정점리스트 거리리스트 총길이 폐합여부)  또는 nil
;;;;==========================================================================
(defun tsp-boundary-path-info (boundary-ent / raw vobj total dlist d ok-d res pt-raw)
  (setq res nil)
  (setq *tsp-cached-bnd-vobj* nil)
  (if boundary-ent
    (progn
      (setq raw (extract-vertices boundary-ent))
      (setq vobj (vl-catch-all-apply 'vlax-ename->vla-object (list boundary-ent)))
      (if (and raw (> (length raw) 1) (not (vl-catch-all-error-p vobj)))
        (progn
          (setq total (vl-catch-all-apply 'vlax-curve-getdistatparam
                        (list vobj (vl-catch-all-apply 'vlax-curve-getendparam (list vobj)))))
          (if (and total (not (vl-catch-all-error-p total)) (> total 0.001))
            (progn
              (setq dlist '())
              (foreach pt-raw raw
                (setq d (vl-catch-all-apply 'vlax-curve-getdistatpoint (list vobj pt-raw)))
                (if (vl-catch-all-error-p d) (setq d nil))
                (setq dlist (cons d dlist))
              )
              (setq dlist (reverse dlist))
              (setq ok-d T)
              (foreach d dlist (if (null d) (setq ok-d nil)))
              (if ok-d
                (progn
                  (setq *tsp-cached-bnd-vobj* vobj)
                  (setq res (list raw dlist total (is-closed-polyline boundary-ent)))
                )
              )
            )
          )
        )
      )
    )
  )
  res
)

;;;;==========================================================================
;;;; defun tsp-dist-of-near-vertex : 정점 리스트에서 pt와 가장 가까운 원본 정점의 누적거리 반환
;;;;   [보강] 모서리 추가(A)나 구간 분할로 인해 pt가 원본 폴리라인의 꼭짓점이 아닌
;;;;   '선분 위 중간점'인 경우에도 폴리라인 투영점 기준 누적거리(Station)를 정밀 반환합니다.
;;;;==========================================================================
(defun tsp-dist-of-near-vertex (raw dlist pt / i d best best-d v vobj cp d-curve)
  (setq best nil best-d 1.0 i 0)
  (if (and raw dlist pt)
    (progn
      ;; 1순위: 원본 폴리라인 꼭짓점(Vertex)과 1.0mm 이내로 일치하는 경우
      (foreach v raw
        (setq d (distance (list (car pt) (cadr pt)) (list (car v) (cadr v))))
        (if (< d best-d)
          (setq best-d d best (nth i dlist))
        )
        (setq i (1+ i))
      )
      ;; 2순위: 꼭짓점이 아닌 선분 위 중간점(모서리 추가/분할점)인 경우 곡선 투영으로 누적거리 산출
      (if (null best)
        (progn
          (setq vobj
            (cond
              ((and (boundp '*tsp-cached-bnd-vobj*) *tsp-cached-bnd-vobj*)
               *tsp-cached-bnd-vobj*)
              ((and (boundp '*tsp-boundary-ent*) *tsp-boundary-ent* (entget *tsp-boundary-ent*))
               (vl-catch-all-apply 'vlax-ename->vla-object (list *tsp-boundary-ent*)))
              (t nil)
            )
          )
          (if (and vobj (not (vl-catch-all-error-p vobj)))
            (progn
              (setq cp (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list vobj (list (car pt) (cadr pt) 0.0))))
              (if (and cp (not (vl-catch-all-error-p cp))
                       (< (distance (list (car pt) (cadr pt)) (list (car cp) (cadr cp))) 500.0))
                (progn
                  (setq d-curve (vl-catch-all-apply 'vlax-curve-getDistAtPoint (list vobj cp)))
                  (if (and d-curve (not (vl-catch-all-error-p d-curve)))
                    (setq best d-curve)
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  best
)

;;;;==========================================================================
;;;; defun tsp-expand-segments-by-geometry : [경계선 추종] 설계 세그먼트를 경계선 실제 형상을 따르는
;;;;   직선 소구간으로 전개한다. 모서리 삭제로 설계 목록에서 빠진 정점은 형상(경계선)에는 그대로
;;;;   유지되므로, 벽체/말뚝/띠장이 삭제된 모서리를 무시한 현(弦)이 아니라 경계선을 따라 작도된다.
;;;;   반환: (새 세그먼트 리스트 부모 인덱스 맵)
;;;;==========================================================================
(defun tsp-expand-segments-by-geometry (seg-list boundary-ent / info raw dlist total is-closed out parent-map i seg p1 p2 d1 d2 fwd bwd use-bwd rev-open wd cand chain prev-pt pt k new-id sub-seg pair brace-entry turn-tol j kept cnt c a b defl)
  (setq out '() parent-map '() new-id 1 i 0)
  (setq info (tsp-boundary-path-info boundary-ent))
  (if (or (null info) (null seg-list))
    (progn
      (if (and seg-list (null info))
        (princ "[경고] 경계선 경로정보 읽기 실패 - 세그먼트가 경계선 형상 대신 직선 기준으로 작도될 수 있습니다"))
      (foreach seg seg-list
        (setq out (append out (list seg)) parent-map (append parent-map (list i)) i (1+ i))
      )
      (list out parent-map)
    )
    (progn
      (setq raw (nth 0 info) dlist (nth 1 info) total (nth 2 info) is-closed (nth 3 info))
      (foreach seg seg-list
        (setq p1 (cdr (assoc 'V-START seg)) p2 (cdr (assoc 'V-END seg)))
        (setq d1 (tsp-dist-of-near-vertex raw dlist p1) d2 (tsp-dist-of-near-vertex raw dlist p2))
        (setq rev-open (and d1 d2 (not is-closed) (< d2 d1)))
        (setq cand '() chain nil)
        (if (and d1 d2 (> (distance p1 p2) 0.001))
          (progn
            (setq fwd (cond ((>= d2 d1) (- d2 d1))
                    (rev-open (- d1 d2))
                    (t (+ (- total d1) d2))))
            (setq bwd (cond (is-closed (- total fwd))
                    (rev-open fwd)
                    (t nil)))
                        (setq use-bwd (if (and bwd (> bwd 0.001) rev-open) T nil))  ;; [B안] 폐합선은 최단 호 대신 설계 진행방향(정방향) 호만 사용
            (setq k 0)
            (foreach pt raw
              (setq wd nil)
              (if use-bwd
                (progn
                  (setq wd (if (<= (nth k dlist) d1) (- d1 (nth k dlist)) (+ d1 (- total (nth k dlist)))))
                  (if (>= wd (- bwd 0.5)) (setq wd nil))
                )
                (progn
                  (setq wd (if (>= (nth k dlist) d1) (- (nth k dlist) d1) (+ (- total d1) (nth k dlist))))
                  (if (>= wd (- fwd 0.5)) (setq wd nil))
                )
              )
              (if (and wd (> wd 0.5)) (setq cand (append cand (list (list wd pt)))))
              (setq k (1+ k))
            )
            (if cand
              (setq cand (vl-sort cand '(lambda (a b) (< (car a) (car b))))
                    chain (mapcar 'cadr cand))
            )
            ;; 180도에 근사한 정점(기본 5도 이내)은 기존 자동제외 규칙과 동일하게 다시 제외
            (if chain
              (progn
                (setq turn-tol (if (and (boundp '*tsp-geo-follow-min-turn-deg*) *tsp-geo-follow-min-turn-deg*)
                                 (* *tsp-geo-follow-min-turn-deg* (/ pi 180.0))
                                 0.0))
                (setq j 0 kept '() cnt (length chain))
                (foreach c chain
                  (setq a (if (= j 0) p1 (nth (1- j) chain)))
                  (setq b (if (= j (1- cnt)) p2 (nth (1+ j) chain)))
                  (setq defl (abs (- (angle a c) (angle c b))))
                  (if (> defl pi) (setq defl (- (* 2.0 pi) defl)))
                  (if (>= defl turn-tol) (setq kept (append kept (list c))))
                  (setq j (1+ j))
                )
                (setq chain kept)
              )
            )
          )
        )
        (if (and chain (>= (length chain) 1))
          (progn
            (setq prev-pt p1 k 0 brace-entry (assoc 'CORNER-BRACE seg))
            (foreach pt (append chain (list p2))
              (setq sub-seg seg)
              (foreach pair (list (cons 'V-START prev-pt) (cons 'V-END pt)
                                  (cons 'ANGLE (angle prev-pt pt)) (cons 'LENGTH (distance prev-pt pt))
                                  (cons 'ID new-id))
                (if (assoc (car pair) sub-seg)
                  (setq sub-seg (subst pair (assoc (car pair) sub-seg) sub-seg))
                  (setq sub-seg (append sub-seg (list pair)))
                )
              )
              (if (and brace-entry (> k 0))
                (setq sub-seg (subst (cons 'CORNER-BRACE (list "N" nil)) brace-entry sub-seg))
              )
              (setq out (append out (list sub-seg)))
              (setq new-id (1+ new-id) k (1+ k) prev-pt pt)
            )
            (setq parent-map (append parent-map (list i)))
          )
          (progn
            (setq out (append out (list seg)) parent-map (append parent-map (list i)))
          )
        )
        (setq i (1+ i))
      )
      (list out parent-map)
    )
  )
)
