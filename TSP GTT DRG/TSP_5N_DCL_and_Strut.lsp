;;; ==========================================================================
;;; 파일명: TSP_5N_DCL_and_Strut.lsp
;;; 구  분: DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part N)
;;; 역  할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-solve-strut-ends, tsp-wale-fix-ends, tsp-get-wale-info-near-pt, tsp-pick-strut-angle-interactive]
;;; 포함 함수: tsp-solve-strut-ends, tsp-wale-fix-ends, tsp-get-wale-info-near-pt, tsp-pick-strut-angle-interactive
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;; 점(pt)과 유한 선분(p1-p2) 사이의 2D 최단 수직거리 계산 (전역 공용 함수)
(defun tsp-pt-seg-dist-2d (pt p1 p2 / dx dy len2 prm)
  (setq pt (list (car pt) (cadr pt))
        p1 (list (car p1) (cadr p1))
        p2 (list (car p2) (cadr p2)))
  (setq dx (- (car p2) (car p1))
        dy (- (cadr p2) (cadr p1)))
  (setq len2 (+ (* dx dx) (* dy dy)))
  (if (< len2 1e-9)
    (distance pt p1)
    (progn
      (setq prm (/ (+ (* (- (car pt) (car p1)) dx) (* (- (cadr pt) (cadr p1)) dy)) len2))
      (setq prm (max 0.0 (min 1.0 prm)))
      (distance pt (list (+ (car p1) (* prm dx)) (+ (cadr p1) (* prm dy))))
    )
  )
)

;; 점(pt)에서 유한 선분(p1-p2)에 수직으로 내린 수선의 발(선분 위 가장 가까운 2D 점) 반환
(defun tsp-closest-pt-on-seg-2d (pt p1 p2 / dx dy len2 prm)
  (setq pt (list (car pt) (cadr pt) 0.0)
        p1 (list (car p1) (cadr p1) 0.0)
        p2 (list (car p2) (cadr p2) 0.0))
  (setq dx (- (car p2) (car p1))
        dy (- (cadr p2) (cadr p1)))
  (setq len2 (+ (* dx dx) (* dy dy)))
  (if (< len2 1e-9)
    p1
    (progn
      (setq prm (/ (+ (* (- (car pt) (car p1)) dx) (* (- (cadr pt) (cadr p1)) dy)) len2))
      (setq prm (max 0.0 (min 1.0 prm)))
      (list (+ (car p1) (* prm dx)) (+ (cadr p1) (* prm dy)) 0.0)
    )
  )
)

;; 특정 지점(pt)이 속한 구간의 실제 경계선 도막 및 실제 띠장 내측선 정보 반환
;;   - 설계 세그먼트의 현(Chord, V-START~V-END)이 중간 절점 생략 등으로 실제 경계선/띠장 선분과
;;     미세한 각도 차이를 가질 때도, 실제 띠장(tsp-wale-lines-geo / tsp-wale-lines-dwg)과
;;     정확히 일치하는 (p1 p2 tang in-norm offset w-p1 w-p2)를 반환하여 수선의 발 오차를 0으로 만듦
(defun tsp-get-local-wale-seg-info (pt seg boundary-ent poly-verts /
   pt-flat seg-info best-info p1 p2 in-norm offset1 w-p1 w-p2 min-d
   wale-lines info bp1 bp2 in-n wp1 wp2 d-b d-w d-cur mid-b
   dwg-lines best-dwg min-dwg-d d-dwg dw-tang dw-norm bnd-p1 bnd-p2)
  (if (null boundary-ent) (setq boundary-ent *tsp-boundary-ent*))
  (setq pt-flat (list (car pt) (cadr pt) 0.0))
  (setq seg-info (tsp-get-seg-inward-normal seg boundary-ent poly-verts))
  (setq best-info seg-info)
  (setq p1 (nth 0 seg-info) p2 (nth 1 seg-info)
        in-norm (nth 3 seg-info) offset1 (nth 4 seg-info)
        w-p1 (nth 5 seg-info) w-p2 (nth 6 seg-info))
  (setq min-d 1e99)

  ;; 1단계: 경계선 추종 확장 세그먼트(tsp-wale-lines-geo)에서 현재 위치의 실제 도막 탐색
  (setq wale-lines (tsp-wale-lines-geo boundary-ent))
  (if (and wale-lines (listp wale-lines))
    (foreach info wale-lines
      (setq bp1 (nth 0 info) bp2 (nth 1 info)
            in-n (nth 3 info)
            wp1 (nth 5 info) wp2 (nth 6 info))
      (if (and bp1 bp2 wp1 wp2 (> (cos (- in-n in-norm)) 0.2))
        (progn
          (setq mid-b (list (/ (+ (car bp1) (car bp2)) 2.0) (/ (+ (cadr bp1) (cadr bp2)) 2.0) 0.0))
          ;; 현재 설계 세그먼트 영역에 속하는 하위 도막인지 판별 (중간 절점 편차 3000mm까지 넉넉히 허용)
          (if (<= (tsp-pt-seg-dist-2d mid-b p1 p2) (+ offset1 3000.0))
            (progn
              (setq d-b (tsp-pt-seg-dist-2d pt-flat bp1 bp2))
              (setq d-w (tsp-pt-seg-dist-2d pt-flat wp1 wp2))
              (setq d-cur (min d-b d-w))
              (if (< d-cur min-d)
                (setq min-d d-cur best-info info)
              )
            )
          )
        )
      )
    )
  )

  ;; 2단계: 도면에 실제 그려진 _띠장(wale) 폴리라인(tsp-wale-lines-dwg) 중 내측선과 대조하여
  ;;         설계 현과 실제 작도된 띠장 사이에 미세 각도 차이가 있으면 실제 띠장 선분으로 정밀 보정
  (setq dwg-lines (tsp-wale-lines-dwg boundary-ent))
  (if (and dwg-lines (listp dwg-lines))
    (progn
      (setq best-dwg nil min-dwg-d 1e99)
      (foreach info dwg-lines
        (setq wp1 (nth 5 info) wp2 (nth 6 info) in-n (nth 3 info))
        (if (and wp1 wp2
                 (> (cos (- in-n (nth 3 best-info))) 0.5)
                 ;; 4줄의 띠장 라인 중 '띠장 내측선(best-info의 w-p1~w-p2)'에 해당하는 선분만 선별
                 (<= (tsp-pt-seg-dist-2d (nth 5 best-info) wp1 wp2) 80.0))
          (progn
            (setq d-dwg (tsp-pt-seg-dist-2d pt-flat wp1 wp2))
            (if (< d-dwg min-dwg-d)
              (setq min-dwg-d d-dwg best-dwg info)
            )
          )
        )
      )
      (if (and best-dwg (<= min-dwg-d (+ offset1 500.0)))
        (progn
          (setq wp1 (nth 5 best-dwg) wp2 (nth 6 best-dwg)
                dw-tang (nth 2 best-dwg) dw-norm (nth 3 best-dwg))
          (setq bnd-p1 (polar wp1 (+ dw-norm pi) offset1)
                bnd-p2 (polar wp2 (+ dw-norm pi) offset1))
          (setq best-info (list bnd-p1 bnd-p2 dw-tang dw-norm offset1 wp1 wp2))
        )
      )
    )
  )
  best-info
)

;; 사용자 클릭점(경계선 또는 띠장)에서 경계선과 실제 띠장 내측선에 수직으로 보낸 선이 만나는 점을
;; 정확히 경계선 시작 기준점(p-base)으로 산출 (p-base에서 다시 in-norm으로 offset1만큼 밀면
;; 클릭한 지점에서 가장 가까운 실제 띠장 내측선 위의 점 = pt1이 오차 0.0000으로 복원됨)
(defun tsp-project-pick-to-seg-boundary (pick-pt seg boundary-ent poly-verts angle-offset-deg /
   pt-flat best-info p1 p2 tang in-norm offset1 w-p1 w-p2 foot-w p-on-bnd vla-bnd p-curve)
  (if (null boundary-ent) (setq boundary-ent *tsp-boundary-ent*))
  (setq pt-flat (list (car pick-pt) (cadr pick-pt) 0.0))
  (if (and seg boundary-ent)
    (progn
      (setq best-info (tsp-get-local-wale-seg-info pt-flat seg boundary-ent poly-verts))
      (setq p1 (nth 0 best-info) p2 (nth 1 best-info)
            tang (nth 2 best-info) in-norm (nth 3 best-info)
            offset1 (nth 4 best-info) w-p1 (nth 5 best-info) w-p2 (nth 6 best-info))
      ;; 1. 클릭점에서 실제 띠장 내측선(w-p1 ~ w-p2)에 내린 수선의 발(가장 가까운 점) 산출
      (setq foot-w (tsp-closest-pt-on-seg-2d pt-flat w-p1 w-p2))
      ;; 2. 그 수선의 발에서 실제 띠장의 법선 역방향(-in-norm)으로 offset1만큼 수직 이동하여 경계선 기준점(p-on-bnd) 확정
      (setq p-on-bnd (polar foot-w (+ in-norm pi) offset1))
            ;; 3. 폴리라인 상의 거리 계산(vlax-curve-getDistAtPoint)을 위해 곡선 위 점이 완벽히 일치할 때만(0.5mm 이내) 스냅
      (setq vla-bnd (vlax-ename->vla-object boundary-ent))
      (setq p-curve (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list vla-bnd p-on-bnd)))
      (if (and p-curve (not (vl-catch-all-error-p p-curve))
               (< (distance p-on-bnd (list (car p-curve) (cadr p-curve) 0.0)) 0.5))
        (list (car p-curve) (cadr p-curve) 0.0)
        p-on-bnd
      )
    )
    (progn
      (setq vla-bnd (vlax-ename->vla-object boundary-ent))
      (setq p-curve (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list vla-bnd pt-flat)))
      (if (and p-curve (not (vl-catch-all-error-p p-curve)))
        (list (car p-curve) (cadr p-curve) 0.0)
        pt-flat
      )
    )
  )
)

;;;;==========================================================================
;;;; defun tsp-find-next-strut-base-by-perp-ctc : [경계선 Chainage 기준] 기준 버팀보의
;;;;   외측 경계선 누적 거리(ref-dist)로부터 경계선을 따라 정확히 step-est(직교 시 target-perp-dist,
;;;;   예: 2000mm)만큼 이동한 다음 경계선 기준점(next-base)을 먼저 확정하고,
;;;;   그 점에서 해당 도막의 국부 띠장 내측선으로 수직 오프셋하여 시작점(next-pt1)을 산출
;;;;   - 세그먼트 내부에 꺾임 절점이 있어도 외측 경계선 거리 및 DRG 전개도 투영 거리가
;;;;     단 0.01mm의 오차도 없이 정확히 target-perp-dist로 일치함
;;;;   반환: (list next-base next-pt1 next-d) 또는 nil
;;;;==========================================================================
(defun tsp-find-next-strut-base-by-perp-ctc (ref-pt1 ref-dist move-sign target-perp-dist seg boundary-ent poly-verts angle-offset-deg /
   vla-bnd is-closed max-dist step-est d-cand p-cand cand-info cand-norm cand-off cand-w1 cand-w2 best-pt1)
  (if (null boundary-ent) (setq boundary-ent *tsp-boundary-ent*))
  (setq vla-bnd (vlax-ename->vla-object boundary-ent))
  (setq is-closed (vlax-curve-isClosed boundary-ent))
  (setq max-dist (vlax-curve-getDistAtParam vla-bnd (vlax-curve-getEndParam vla-bnd)))
  (setq step-est (tsp-calc-strut-step-dist target-perp-dist angle-offset-deg))
  (setq d-cand (+ ref-dist (* move-sign step-est)))
  (if (< d-cand 0.0) (setq d-cand (if is-closed (+ max-dist d-cand) nil)))
  (if (and d-cand (> d-cand max-dist)) (setq d-cand (if is-closed (- d-cand max-dist) nil)))

  (if d-cand
    (progn
      (setq p-cand (vlax-curve-getPointAtDist boundary-ent d-cand))
      (setq p-cand (list (car p-cand) (cadr p-cand) 0.0))
      (setq cand-info (tsp-get-local-wale-seg-info p-cand seg boundary-ent poly-verts))
      (setq cand-norm (nth 3 cand-info)
            cand-off  (nth 4 cand-info)
            cand-w1   (nth 5 cand-info)
            cand-w2   (nth 6 cand-info))
      (setq best-pt1 (tsp-closest-pt-on-seg-2d (polar p-cand cand-norm cand-off) cand-w1 cand-w2))
      (list p-cand best-pt1 d-cand)
    )
    nil
  )
)

;;;;==========================================================================
;;;; defun tsp-refine-strut-manual-pts : 저장된 버팀보 시작점 리스트(s-manual-pts)를
;;;;   외측 경계선(_경계선(Boundary)) 누적 거리(Chainage) 기준으로 정밀 보정하여 반환
;;;;   - 기존 내측 띠장 기준으로 저장된 CSV 좌표(꺾임 구간 +9.54mm 오차 포함)나
;;;;     유효숫자 반올림 좌표를 첫 번째 점의 경계선 누적거리 기준으로 정확히 복원
;;;;==========================================================================
(defun tsp-refine-strut-manual-pts (manual-pts ctc-dist seg boundary-ent poly-verts angle-offset-deg /
   vla-bnd is-closed max-dist step-est n-pts out cur-pt p-base0 p-curve0 prev-d
   cur-info cur-norm cur-off cur-w1 cur-w2 prev-pt1
   p-base-c p-curve-c d-cur delta-d move-sign abs-d-bnd d-2d k-mult d-new p-new refined-pt1 idx)
  (if (null boundary-ent) (setq boundary-ent *tsp-boundary-ent*))
  (if (or (null manual-pts) (< (length manual-pts) 2) (<= ctc-dist 10.0) (null boundary-ent))
    manual-pts
    (progn
      (setq vla-bnd (vlax-ename->vla-object boundary-ent))
      (setq is-closed (vlax-curve-isClosed boundary-ent))
      (setq max-dist (vlax-curve-getDistAtParam vla-bnd (vlax-curve-getEndParam vla-bnd)))
      (setq step-est (tsp-calc-strut-step-dist ctc-dist angle-offset-deg))

      ;; 1. 첫 번째 기준 버팀보 시작점의 외측 경계선 기준점(p-base0) 및 누적거리(prev-d) 산출
      (setq cur-pt (car manual-pts))
      (setq p-base0 (tsp-project-pick-to-seg-boundary cur-pt seg boundary-ent poly-verts angle-offset-deg))
      (setq p-curve0 (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list vla-bnd p-base0)))
      (setq prev-d
        (if (and p-curve0 (not (vl-catch-all-error-p p-curve0)))
          (vlax-curve-getDistAtPoint vla-bnd p-curve0)
          0.0
        )
      )
      (setq cur-info (tsp-get-local-wale-seg-info p-base0 seg boundary-ent poly-verts))
      (setq cur-norm (nth 3 cur-info)
            cur-off  (nth 4 cur-info)
            cur-w1   (nth 5 cur-info)
            cur-w2   (nth 6 cur-info))
      (setq prev-pt1 (tsp-closest-pt-on-seg-2d (polar p-base0 cur-norm cur-off) cur-w1 cur-w2))
      (setq out (list prev-pt1))
      (setq idx 1 n-pts (length manual-pts))

      ;; 2. 이어지는 각 점들에 대해 이전 경계선 누적거리(prev-d)와의 차이가 step-est의 정수배(25mm 오차 이내)이면
      ;;    외측 경계선 누적거리가 정확히 (prev-d +/- k * step-est)가 되는 지점으로 정밀 복원
      (while (< idx n-pts)
        (setq cur-pt (nth idx manual-pts))
        (setq p-base-c (tsp-project-pick-to-seg-boundary cur-pt seg boundary-ent poly-verts angle-offset-deg))
        (setq p-curve-c (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list vla-bnd p-base-c)))
        (setq d-cur
          (if (and p-curve-c (not (vl-catch-all-error-p p-curve-c)))
            (vlax-curve-getDistAtPoint vla-bnd p-curve-c)
            prev-d
          )
        )
        (setq delta-d (- d-cur prev-d))
        (if is-closed
          (cond
            ((> delta-d (/ max-dist 2.0)) (setq delta-d (- delta-d max-dist)))
            ((< delta-d (- (/ max-dist 2.0))) (setq delta-d (+ delta-d max-dist)))
          )
        )
        (setq move-sign (if (>= delta-d 0.0) 1.0 -1.0))
        (setq abs-d-bnd (abs delta-d))
        (setq d-2d (distance (list (car prev-pt1) (cadr prev-pt1)) (list (car cur-pt) (cadr cur-pt))))
        (setq k-mult (fix (+ (/ (max abs-d-bnd d-2d) (max 1.0 step-est)) 0.5)))

        (if (and (>= k-mult 1)
                 (or (<= (abs (- abs-d-bnd (* k-mult step-est))) 25.0)
                     (<= (abs (- d-2d (* k-mult step-est))) 25.0)))
          (progn
            (setq d-new (+ prev-d (* move-sign k-mult step-est)))
            (if (< d-new 0.0) (setq d-new (if is-closed (+ max-dist d-new) 0.0)))
            (if (> d-new max-dist) (setq d-new (if is-closed (- d-new max-dist) max-dist)))
            (setq p-new (vlax-curve-getPointAtDist boundary-ent d-new))
            (setq p-new (list (car p-new) (cadr p-new) 0.0))
            (setq cur-info (tsp-get-local-wale-seg-info p-new seg boundary-ent poly-verts))
            (setq cur-norm (nth 3 cur-info)
                  cur-off  (nth 4 cur-info)
                  cur-w1   (nth 5 cur-info)
                  cur-w2   (nth 6 cur-info))
            (setq refined-pt1 (tsp-closest-pt-on-seg-2d (polar p-new cur-norm cur-off) cur-w1 cur-w2))
            (setq prev-d d-new)
          )
          (progn
            (setq cur-info (tsp-get-local-wale-seg-info p-base-c seg boundary-ent poly-verts))
            (setq cur-norm (nth 3 cur-info)
                  cur-off  (nth 4 cur-info)
                  cur-w1   (nth 5 cur-info)
                  cur-w2   (nth 6 cur-info))
            (setq refined-pt1 (tsp-closest-pt-on-seg-2d cur-pt cur-w1 cur-w2))
            (setq prev-d d-cur)
          )
        )
        (setq out (append out (list refined-pt1)))
        (setq prev-pt1 refined-pt1)
        (setq idx (1+ idx))
      )
      out
    )
  )
)

;; 축과 띠장 내측선의 교점으로 양단 접합점 산출
(defun tsp-solve-strut-ends (pt1 ray-ang boundary-ent wale-lines offset1 offset2 poly-verts /
   pr info wp1 wp2 ip ipn d d-seg in-norm best-near best-near-d best-far best-far-d best-opp m cosv)
  (setq best-near nil best-near-d 1e99 best-far nil best-far-d 1e99 best-opp nil)
  ;; 광선은 '충분히 긴 유한 구간'으로 두고, 띠장은 반드시 자기 구간 내에서만 교차 판정한다.
  ;;  (확장 직선으로 교차시키면 구간을 벗어난 다른 띠장에 붙어 길이가 제각각이 된다)
  (setq pr (polar pt1 ray-ang 1e6))
  (if (and wale-lines (> (length wale-lines) 1))
    (progn
      ;; 1) 근측 : pt1 이 실제로 놓여 있는 띠장 (유한 선분 최단거리 + 내측 법선 방향 일치 검증)
      (foreach info wale-lines
        (setq in-norm (nth 3 info) wp1 (nth 5 info) wp2 (nth 6 info))
        (if (and wp1 wp2 in-norm (> (cos (- in-norm ray-ang)) 0.15))
          (progn
            (setq d-seg (tsp-pt-seg-dist-2d pt1 wp1 wp2))
            (if (and (< d-seg best-near-d) (< d-seg (+ (* offset1 2.0) 1000.0)))
              (setq best-near-d d-seg best-near info)
            )
          )
        )
      )
      ;; 2) 원측 : 광선이 '실제 띠장 구간'과 만나는 첫 교점 (유한구간 교차만 허용)
      (if best-near
        (foreach info wale-lines
          (if (not (eq info best-near))
            (progn
              (setq wp1 (nth 5 info) wp2 (nth 6 info))
              (if (and wp1 wp2)
                (progn
                  (setq ip (inters pt1 pr wp1 wp2 T))
                  (if ip
                    (progn
                      (setq d (distance pt1 ip))
                      (setq cosv (cos (- (angle pt1 ip) ray-ang)))
                      (setq m (list (/ (+ (car pt1) (car ip)) 2.0)
                                    (/ (+ (cadr pt1) (cadr ip)) 2.0) 0.0))
                      (if (and (> cosv 0.5) (> d 10.0)
                               (tsp-is-point-inside m poly-verts) (< d best-far-d))
                        (setq best-far-d d best-far ip
                              best-opp (inters pt1 pr (nth 0 info) (nth 1 info) nil))
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
                    (if (and best-near best-far)
    (progn
      ;; [핵심 수정] 광선(ray-ang)과의 사선 교차점이 아니라,
      ;; 클릭/기준점에서 가장 가까운 실제 띠장 내측선 위의 점(수선의 발)이 정확히 시작점(pt1)이 되도록 수직 투영
      (setq ipn (tsp-closest-pt-on-seg-2d pt1 (nth 5 best-near) (nth 6 best-near)))
      (if (and ipn (< (distance pt1 ipn) (+ offset1 500.0)))
        (setq pt1 ipn)
      )
      ;; 확정된 시작점(pt1)에서 다시 ray-ang 방향으로 원측 띠장 교점(best-far)을 정밀 재산출
      (setq pr (polar pt1 ray-ang 1e6))
      (setq best-far nil best-far-d 1e99 best-opp nil)
      (foreach info wale-lines
        (if (not (eq info best-near))
          (progn
            (setq wp1 (nth 5 info) wp2 (nth 6 info))
            (if (and wp1 wp2)
              (progn
                (setq ip (inters pt1 pr wp1 wp2 T))
                (if ip
                  (progn
                    (setq d (distance pt1 ip))
                    (setq cosv (cos (- (angle pt1 ip) ray-ang)))
                    (setq m (list (/ (+ (car pt1) (car ip)) 2.0)
                                  (/ (+ (cadr pt1) (cadr ip)) 2.0) 0.0))
                    (if (and (> cosv 0.5) (> d 10.0)
                             (tsp-is-point-inside m poly-verts) (< d best-far-d))
                      (setq best-far-d d best-far ip
                            best-opp (inters pt1 pr (nth 0 info) (nth 1 info) nil))
                    )
                  )
                )
              )
            )
          )
        )
      )
      (if best-far
        (progn
          (if (null best-opp) (setq best-opp best-far))
          (list pt1 best-far best-opp)
        )
        nil
      )
    )
    nil
  )
)

;; 1안 우선, 실패 시 2안 폴백
(defun tsp-wale-fix-ends (pt1 ray-ang boundary-ent offset1 offset2 poly-verts / res)
  (setq res (tsp-solve-strut-ends pt1 ray-ang boundary-ent
                                  (tsp-wale-lines-geo boundary-ent) offset1 offset2 poly-verts))
  (if (null res)
    (setq res (tsp-solve-strut-ends pt1 ray-ang boundary-ent
                                    (tsp-wale-lines-dwg boundary-ent) offset1 offset2 poly-verts))
  )
  res
)

;; 버팀보 끝점(pt) 및 굴착면 내측 기대 방향(expected-in-dir)으로부터 해당 세그먼트의 띠장 내측선 정보 반환
(defun tsp-get-wale-info-near-pt (pt expected-in-dir wale-lines / poly-verts best-info min-score seg info w-p1 w-p2 in-norm d-perp d-seg score)
  (setq poly-verts (tsp-build-poly-verts *tsp-boundary-ent*))
  (setq best-info nil min-score 1e99)
  (if (null wale-lines)
    (setq wale-lines
      (if (and (boundp '*segment-list*) *segment-list*)
        (mapcar '(lambda (sg) (tsp-get-seg-inward-normal sg *tsp-boundary-ent* poly-verts)) *segment-list*)
        nil
      )
    )
  )
  (if (and wale-lines (listp wale-lines))
    (foreach info wale-lines
      (if info
       (progn
        (setq in-norm (nth 3 info) w-p1 (nth 5 info) w-p2 (nth 6 info))
      (if (> (cos (- in-norm expected-in-dir)) -0.15)
        (progn
          (setq d-perp (abs (tsp-project-dist w-p1 in-norm pt)))
          (setq d-seg (abs (- (+ (distance w-p1 pt) (distance pt w-p2)) (distance w-p1 w-p2))))
          (setq score (+ d-perp (* 0.25 d-seg)))
          (if (< score min-score)
            (setq min-score score best-info info)
          )
        )
       )
      )
    )
    )
  )
  (if best-info
    best-info
    (list pt (polar pt (+ expected-in-dir (/ pi 2.0)) 1000.0) (+ expected-in-dir (/ pi 2.0)) expected-in-dir 300.0 pt (polar pt (+ expected-in-dir (/ pi 2.0)) 1000.0))
  )
)

;; 다이얼로그 [각도 지정(끝점 클릭)] 버튼 실행 함수 (시작점 고정 -> 끝점 클릭으로 버팀보 방향 결정)
(defun tsp-pick-strut-angle-interactive (s-data / poly-verts seg-idx cur-seg seg-info in-norm1 offset1 manual-pts start-mode ctc-dist old-ang-off new-ang-off p-start-v p-end-v p-start-wcs p-end-wcs mid-pt p-base pt1-ref pick-pt raw-ang delta-rad s-brace-opt vla-bnd total-len d0 old-step new-step new-manual-pts p-on-c d-cur k-idx d-new p-new)
        (setq seg-idx (if (and (boundp '*tsp-current-seg-idx*) *tsp-current-seg-idx*) *tsp-current-seg-idx* 0))
  (setq cur-seg (nth seg-idx *segment-list*))
  (setq poly-verts (tsp-build-poly-verts *tsp-boundary-ent*))
  (setq manual-pts (if (> (length s-data) 8) (nth 8 s-data) nil))
  (setq seg-info
    (if (and manual-pts (> (length manual-pts) 0) (listp (car manual-pts)))
      (tsp-get-local-wale-seg-info (car manual-pts) cur-seg *tsp-boundary-ent* poly-verts)
      (tsp-get-seg-inward-normal cur-seg *tsp-boundary-ent* poly-verts)
    )
  )
  (setq in-norm1 (nth 3 seg-info) offset1 (nth 4 seg-info))
  (setq manual-pts (if (> (length s-data) 8) (nth 8 s-data) nil))
  (setq start-mode (if (> (length s-data) 9) (nth 9 s-data) "1"))
  (setq ctc-dist (tsp-ctc-mm (if (> (length s-data) 10) (nth 10 s-data) "5.0")))
  (setq s-brace-opt (if (> (length s-data) 11) (nth 11 s-data) nil))
  (setq old-ang-off (atof (tsp-brace-opt-get s-brace-opt 14 "0.0")))

    ;; 버팀보 시작점(pt1-ref) 결정: 기존 작도 시작점 -> 세그먼트 중점 -> 사용자 클릭 순 (비수평 세그먼트 법선 역투영 적용)
  (cond
    ((and manual-pts (> (length manual-pts) 0) (listp (car manual-pts)))
     (setq p-base (tsp-project-pick-to-seg-boundary (car manual-pts) cur-seg *tsp-boundary-ent* poly-verts old-ang-off))
     (setq pt1-ref (polar (list (car p-base) (cadr p-base) 0.0) in-norm1 offset1))
    )
    ((or (= start-mode "1") (= start-mode "2"))
     (setq p-start-v (cdr (assoc 'V-START cur-seg)) p-end-v (cdr (assoc 'V-END cur-seg)))
     (setq p-start-wcs (trans p-start-v *tsp-boundary-ent* 0) p-end-wcs (trans p-end-v *tsp-boundary-ent* 0))
     (setq mid-pt (list (/ (+ (car p-start-wcs) (car p-end-wcs)) 2.0) (/ (+ (cadr p-start-wcs) (cadr p-end-wcs)) 2.0) 0.0))
     (setq p-base (tsp-project-pick-to-seg-boundary mid-pt cur-seg *tsp-boundary-ent* poly-verts old-ang-off))
     (setq pt1-ref (polar (list (car p-base) (cadr p-base) 0.0) in-norm1 offset1))
    )
    (t
     (princ "\n[버팀보 각도 지정] 먼저 버팀보 시작 기준점(경계선 또는 띠장 근처)을 클릭하세요 (Enter: 세그먼트 중점): ")
     (setq pick-pt (vl-catch-all-apply 'getpoint))
     (if (and (not (vl-catch-all-error-p pick-pt)) pick-pt)
       (setq p-base (tsp-project-pick-to-seg-boundary (trans pick-pt 1 0) cur-seg *tsp-boundary-ent* poly-verts old-ang-off))
       (progn
         (setq p-start-v (cdr (assoc 'V-START cur-seg)) p-end-v (cdr (assoc 'V-END cur-seg)))
         (setq p-start-wcs (trans p-start-v *tsp-boundary-ent* 0) p-end-wcs (trans p-end-v *tsp-boundary-ent* 0))
         (setq mid-pt (list (/ (+ (car p-start-wcs) (car p-end-wcs)) 2.0) (/ (+ (cadr p-start-wcs) (cadr p-end-wcs)) 2.0) 0.0))
         (setq p-base (tsp-project-pick-to-seg-boundary mid-pt cur-seg *tsp-boundary-ent* poly-verts old-ang-off))
       )
     )
     (setq pt1-ref (polar (list (car p-base) (cadr p-base) 0.0) in-norm1 offset1))
    )
  )

  (initget "U u")
  (setq pick-pt (vl-catch-all-apply 'getpoint (list pt1-ref "\n[버팀보 각도 지정] 시작점으로부터 버팀보가 향할 끝점(방향)을 클릭하세요 [90도 직교 초기화(U) / 취소(ESC)]: ")))
  (if (and (not (vl-catch-all-error-p pick-pt)) pick-pt)
    (progn
      (setq new-ang-off old-ang-off)
      (if (or (= pick-pt "U") (= pick-pt "u"))
        (progn
          (setq new-ang-off 0.0)
          (princ "\n>> 버팀보 각도가 띠장 직교(90.0°)로 초기화되었습니다.")
        )
        (progn
          (setq raw-ang (angle pt1-ref (list (car pick-pt) (cadr pick-pt) 0.0)))
          (setq delta-rad (tsp-norm-angle-rad (- raw-ang in-norm1)))
          (if (>= (abs delta-rad) (* 75.0 (/ pi 180.0)))
            (alert "띠장 직교 방향 대비 회전각이 너무 큽니다 (±75° 이내로 지정해주세요).")
            (progn
              (setq new-ang-off (* (/ delta-rad pi) 180.0))
              (princ (strcat "\n>> 버팀보 방향각 설정 완료: 띠장 직교 기준 " (if (>= new-ang-off 0.0) "+" "") (rtos new-ang-off 2 1) "°"))
            )
          )
        )
      )
      (setq s-brace-opt (tsp-brace-opt-set s-brace-opt 14 (rtos new-ang-off 2 4)))
            ;; 이미 배치된 버팀보 그룹(manual-pts)이 있으면 직교 C.T.C 간격 기준으로 경계선 위치 재정렬 (다중 절점 도막의 국부 법선/띠장 반영)
      (if (and manual-pts (> (length manual-pts) 1))
        (progn
          (setq vla-bnd (vlax-ename->vla-object *tsp-boundary-ent*))
          (setq total-len (vlax-curve-getDistAtParam vla-bnd (vlax-curve-getEndParam vla-bnd)))
          (setq old-step (tsp-calc-strut-step-dist ctc-dist old-ang-off))
          (setq new-step (tsp-calc-strut-step-dist ctc-dist new-ang-off))
          (setq p-on-c (tsp-project-pick-to-seg-boundary (car manual-pts) cur-seg *tsp-boundary-ent* poly-verts old-ang-off))
          (setq d0 (vlax-curve-getDistAtPoint vla-bnd p-on-c))
          (setq new-manual-pts '())
          (foreach pt manual-pts
            (setq p-on-c (tsp-project-pick-to-seg-boundary pt cur-seg *tsp-boundary-ent* poly-verts old-ang-off))
            (setq d-cur (vlax-curve-getDistAtPoint vla-bnd p-on-c))
            (setq k-idx (fix (+ (/ (abs (- d-cur d0)) (max 1.0 old-step)) 0.5)))
            (if (< (- d-cur d0) 0.0) (setq k-idx (- k-idx)))
            (setq d-new (+ d0 (* k-idx new-step)))
            (if (and (>= d-new 0.0) (<= d-new total-len))
              (progn
                (setq p-new (vlax-curve-getPointAtDist vla-bnd d-new))
                (setq p-new (list (car p-new) (cadr p-new) 0.0))
                (setq seg-info (tsp-get-local-wale-seg-info p-new cur-seg *tsp-boundary-ent* poly-verts))
                (setq new-manual-pts
                  (append new-manual-pts
                    (list (tsp-closest-pt-on-seg-2d (polar p-new (nth 3 seg-info) (nth 4 seg-info)) (nth 5 seg-info) (nth 6 seg-info)))))
              )
            )
          )
          (if (> (length new-manual-pts) 0) (setq manual-pts new-manual-pts))
        )
      )
      (setq s-data (update-list-item s-data 8 manual-pts))
      (setq s-data (update-list-item s-data 11 s-brace-opt))
    )
  )
  s-data
)
