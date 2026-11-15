;;; ==========================================================================
;;; 파일명: TSP_4K_Plan.lsp
;;; 구  분: 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part K)
;;; 역  할: 세그먼트/모서리 번호표(캡슐) 작도 및 사보강재 한계거리 산출 [draw-segment-numbers, draw-corner-numbers, draw-capsule-shape, check-wedge-collision, check-boundary-collision, tag-last-corner-ent, tsp-calculate-brace-limit]
;;; 포함 함수: draw-segment-numbers, draw-corner-numbers, draw-capsule-shape, check-wedge-collision, check-boundary-collision, tag-last-corner-ent, tsp-calculate-brace-limit
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun draw-segment-numbers : 정렬된 vertices를 인자로 받아 세그먼트 번호 표기
;;;;==========================================================================
(defun draw-segment-numbers (boundary-ent boundary-orient vertices / is-closed num-verts max-segments seg-idx pt1 pt2 mid-pt seg-angle inward-angle text-pt text-h offset-dist layer-name circle-rad current-scale seg-data)
  (setq current-scale (if (and *tsp-scale-plan* (> *tsp-scale-plan* 0)) *tsp-scale-plan* 600.0))
  (princ (strcat "\n[INFO] 현재 적용 스케일: 1/" (rtos current-scale 2 0)))
  (setq text-h (* current-scale 2.5))
  (setq offset-dist (* current-scale 2.5))
  (setq circle-rad (* text-h 0.8))
  (setq layer-name "_번호")
  (create-layer-if-not-exists layer-name "2")
  (setq is-closed (is-closed-polyline boundary-ent))
  (setq num-verts (length vertices))
  (setq max-segments (if is-closed num-verts (- num-verts 1)))
  (setq seg-idx 0)
  (while (< seg-idx max-segments)
    (setq seg-data (nth seg-idx *segment-list*))

    ;; 세그먼트에 데이터 입력 여부(IS-DEFINED)와 상관없이
    ;; 사용자가 경계선을 선택한 직후 무조건 번호표를 그려서 시각적으로 확인 가능하도록 조건문 해제
    (setq pt1 (nth seg-idx vertices))
    (setq pt2 (nth (if is-closed (rem (+ seg-idx 1) num-verts) (+ seg-idx 1)) vertices))
    (setq mid-pt (list (/ (+ (car pt1) (car pt2)) 2.0) (/ (+ (cadr pt1) (cadr pt2)) 2.0) 0.0))
    (setq seg-angle (angle pt1 pt2))
    (setq inward-angle (+ seg-angle (* boundary-orient (/ pi 2.0))))
    (setq text-pt (polar mid-pt inward-angle offset-dist))

    ;; 1. 텍스트 객체 생성 및 투명도 60 적용
    (entmake (list '(0 . "TEXT") (cons 8 layer-name) (cons 62 2)
                   (cons 10 text-pt) (cons 40 text-h)
                   (cons 1 (itoa (1+ seg-idx)))
                   '(72 . 4) (cons 11 text-pt)))
    (vla-put-EntityTransparency (vlax-ename->vla-object (entlast)) "60")

    ;; 2. 원(Circle) 객체 생성 및 투명도 60 적용
    (entmake (list '(0 . "CIRCLE") (cons 8 layer-name) (cons 62 2)
                   (cons 10 text-pt) (cons 40 circle-rad)))
    (vla-put-EntityTransparency (vlax-ename->vla-object (entlast)) "60")

    (setq seg-idx (1+ seg-idx))
  )
)

;;;;==========================================================================
;;;; defun draw-corner-numbers : 모서리 지시선 작도
;;;;==========================================================================
(defun draw-corner-numbers (boundary-ent boundary-orient vertices / is-closed num-verts i v-prev v-curr v-next p1-s p1-e p2-s p2-e n1 n2 offset-pt angle-dir line-start line1-end cap-center cap-top cap-bot line2-end circle-center text-h-num text-h-cnt layer-name text-val current-scale s-factor cap-w-half cap-h-half cap-rad angle-perp offset-dist line1-len line2-len circle-rad text-angle is-convex base-data final-data wedge-len wedge-half-ang cur-item prev-idx next-idx prev-item next-item ang-to-prev ang-to-next diff orig-dist cumulative-counts current-count p-seg-idx c-seg-idx p-def c-def is-active max-segments prev-active next-active is-open-end can-rotate seg-mid-count total-count cur-cum-count fully-closed active-flags mid-line-len cap1-bot cap1-center cap1-top cap2-bot cap2-center cap2-top draw-capsule-shape check-wedge-collision evade-angles orig-angle-dir test-idx evade-sign)

;;;;==========================================================================
;;;; defun draw-capsule-shape : 모서리 번호 캡슐 작도
;;;;==========================================================================
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
                   (cons 10 p-c1) '(42 . 0.41421356) (cons 10 p-c2) '(42 . 0.0)
                   (cons 10 p-c3) '(42 . 0.41421356) (cons 10 p-c4) '(42 . 0.0)
                   (cons 10 p-c5) '(42 . 0.41421356) (cons 10 p-c6) '(42 . 0.0)
                   (cons 10 p-c7) '(42 . 0.41421356) (cons 10 p-c8) '(42 . 0.0)))
  )

;;;;==========================================================================
;;;; defun check-wedge-collision : 모서리 번호표 간섭 검사 (한계치 3000으로 완화)
;;;;==========================================================================
  (defun check-wedge-collision (pt1 ang1 pt2 ang2 len half-ang s-factor / p1-L p1-R p2-L p2-R p1-C p2-C res C1 C2 dist-limit)
    (setq p1-L (polar pt1 (+ ang1 half-ang) len))
    (setq p1-R (polar pt1 (- ang1 half-ang) len))
    (setq p2-L (polar pt2 (+ ang2 half-ang) len))
    (setq p2-R (polar pt2 (- ang2 half-ang) len))
    (setq p1-C (polar pt1 ang1 len))
    (setq p2-C (polar pt2 ang2 len))
    (setq res nil)
    (if (inters pt1 p1-L pt2 p2-R) (setq res T))
    (if (inters pt1 p1-R pt2 p2-L) (setq res T))
    (if (inters pt1 p1-L pt2 p2-L) (setq res T))
    (if (inters pt1 p1-R pt2 p2-R) (setq res T))
    (if (inters pt1 p1-C pt2 p2-C) (setq res T))
    (setq C1 (polar pt1 ang1 (* 4000.0 s-factor)))
    (setq C2 (polar pt2 ang2 (* 4000.0 s-factor)))
    ;; 번호표 캡슐 간 거리를 기존 4500에서 2500으로 줄여 합리적인 45도 회피가 기각되지 않도록 조정
    (setq dist-limit (* 2500.0 s-factor))
    (if (< (distance C1 C2) dist-limit) (setq res T))
    res
  )

;;;;==========================================================================
;;;; defun check-boundary-collision : 지시선이 경계선과 교차하거나 완전히 겹치는지 검사
;;;;==========================================================================
  (defun check-boundary-collision (v-pt ang v-prev v-next / p-start p-end res ang-p ang-n diff-p diff-n)
    ;; 정점(Vertex)에서 10mm 띄워서 시작하여 원점 겹침 오류(False Positive) 방지
    (setq p-start (polar v-pt ang 10.0))
    (setq p-end (polar v-pt ang 5000.0))
    (setq res nil)

    ;; 1. 선분 교차 검사 (경계선 내부를 직접적으로 파고드는 경우)
    (if v-prev (if (inters p-start p-end v-pt v-prev T) (setq res T)))
    (if v-next (if (inters p-start p-end v-pt v-next T) (setq res T)))

    ;; 2. 평행 겹침 검사 (지시선이 경계선과 5도 이내로 나란하게 그려지는 경우 차단)
    (if (not res)
      (progn
        (if v-prev
          (progn
            (setq ang-p (angle v-pt v-prev))
            (setq diff-p (abs (- ang ang-p)))
            (while (> diff-p pi) (setq diff-p (abs (- diff-p (* 2.0 pi)))))
            (if (< diff-p (* 5.0 (/ pi 180.0))) (setq res T))
          )
        )
        (if v-next
          (progn
            (setq ang-n (angle v-pt v-next))
            (setq diff-n (abs (- ang ang-n)))
            (while (> diff-n pi) (setq diff-n (abs (- diff-n (* 2.0 pi)))))
            (if (< diff-n (* 5.0 (/ pi 180.0))) (setq res T))
          )
        )
      )
    )
    res
  )
  (setq current-scale (if (and *tsp-scale-plan* (> *tsp-scale-plan* 0)) *tsp-scale-plan* 600.0))
  (setq s-factor (/ current-scale 600.0))
  (setq layer-name "_지시")
  (create-layer-if-not-exists layer-name "2")
  (setq offset-dist (* 1000.0 s-factor))
  (setq line1-len (* 1300.0 s-factor))
  (setq cap-h-half (* 600.0 s-factor))
  (setq cap-w-half (* 1195.0 s-factor))
  (setq cap-rad (* 480.0 s-factor))
  (setq line2-len (* 1300.0 s-factor))
  (setq circle-rad (* 1200.0 s-factor))
  (setq text-h-num (* 840.0 s-factor))
  (setq text-h-cnt (* 720.0 s-factor))
  (setq wedge-len (* 7614.0 s-factor))
  (setq wedge-half-ang 0.245)
  (setq is-closed (is-closed-polyline boundary-ent))
  (setq num-verts (length vertices))
  (setq max-segments (if is-closed num-verts (- num-verts 1)))
  (setq fully-closed is-closed)
  (if is-closed
    (foreach seg *segment-list*
      (if (not (cdr (assoc 'IS-DEFINED seg))) (setq fully-closed nil))
    )
  )
  (setq cumulative-counts '())
  (setq current-count 0)
  (setq active-flags '())
  (setq i 0)
  (while (< i num-verts)
    (setq p-seg-idx (if (= i 0) (if is-closed (1- num-verts) -1) (1- i)))
    (setq c-seg-idx (if (>= i max-segments) -1 i))
      (setq p-def (if (>= p-seg-idx 0) (cdr (assoc 'IS-DEFINED (nth p-seg-idx *segment-list*))) nil))
      (setq c-def (if (>= c-seg-idx 0) (cdr (assoc 'IS-DEFINED (nth c-seg-idx *segment-list*))) nil))
      (setq is-active (or p-def c-def))
      (setq active-flags (append active-flags (list (cons i is-active))))
     (if is-active
        (progn
          (setq p-type (if (and (>= p-seg-idx 0) p-def) (cdr (assoc 'WALL-TYPE (nth p-seg-idx *segment-list*))) "NONE"))
          (setq c-type (if (and (>= c-seg-idx 0) c-def) (cdr (assoc 'WALL-TYPE (nth c-seg-idx *segment-list*))) "NONE"))
          ;; C.I.P 벽체의 시작점인지 판별 (i=0 이거나, 이전 세그먼트가 CIP가 아닌 경우)
          (setq is-cip-start (and (= c-type "CIP") (or (= i 0) (not (= p-type "CIP")))))

          ;; H-Pile이 포함된 코너이거나, C.I.P 코너(시작점 포함 모든 CIP 코너)일 경우 코너 카운트 +1
          (if (or (= p-type "HPILE") (= c-type "HPILE") (= c-type "CIP"))
            (setq current-count (1+ current-count))
          )

          (setq cumulative-counts (append cumulative-counts (list (cons i current-count))))

          (if c-def
            (progn
              (setq seg-mid-count (nth c-seg-idx *tsp-segment-pile-counts*))
              (if (null seg-mid-count) (setq seg-mid-count 0))

              ;; C.I.P 세그먼트 카운트(counts)에는 코너말뚝이 포함되어 있으므로
              ;; 위 +1과 상쇄시키기 위해 1 차감 (H-Pile 세그먼트에는 적용하지 않음)
              (if (and (= c-type "CIP") (> seg-mid-count 0))
                (setq seg-mid-count (1- seg-mid-count))
              )

              (setq current-count (+ current-count seg-mid-count))
            )
          )

        )
      )
    (setq i (1+ i))
  )
  (setq total-count current-count)
  (setq base-data '())
  (setq i 0)
  (while (< i num-verts)
    (setq v-curr (nth i vertices))
    (if is-closed
      (progn (setq v-prev (nth (if (= i 0) (1- num-verts) (1- i)) vertices)) (setq v-next (nth (if (= i (1- num-verts)) 0 (1+ i)) vertices)))
      (progn (setq v-prev (if (= i 0) nil (nth (1- i) vertices))) (setq v-next (if (= i (1- num-verts)) nil (nth (1+ i) vertices))))
    )
    (setq is-convex nil)
    (if (and v-prev v-next) (setq is-convex (is-corner-convex v-prev v-curr v-next boundary-orient)))
    (setq is-open-end (not (and v-prev v-next)))
    (setq offset-pt nil)
    (if (and v-prev v-next)
      (progn
        (setq n1 (- (angle v-prev v-curr) (* boundary-orient (/ pi 2.0))))
        (setq n2 (- (angle v-curr v-next) (* boundary-orient (/ pi 2.0))))
        (setq p1-s (polar v-prev n1 offset-dist)) (setq p1-e (polar v-curr n1 offset-dist))
        (setq p2-s (polar v-curr n2 offset-dist)) (setq p2-e (polar v-next n2 offset-dist))
        (setq offset-pt (inters p1-s p1-e p2-s p2-e nil))
        (if (null offset-pt) (setq offset-pt (polar v-curr n1 offset-dist)))
      )
      (if v-next
        (progn (setq n2 (- (angle v-curr v-next) (* boundary-orient (/ pi 2.0)))) (setq offset-pt (polar v-curr n2 offset-dist)))
        (progn (setq n1 (- (angle v-prev v-curr) (* boundary-orient (/ pi 2.0)))) (setq offset-pt (polar v-curr n1 offset-dist)))
      )
    )
    (setq angle-dir (angle v-curr offset-pt))
    (setq is-active (cdr (assoc i active-flags)))
    (setq base-data (append base-data (list (list offset-pt angle-dir is-convex v-curr is-active is-open-end))))
    (setq i (1+ i))
  )
  (setq final-data '())
  (setq i 0)
  (while (< i num-verts)
    (setq cur-item (nth i base-data))
    (setq offset-pt (car cur-item)) (setq angle-dir (cadr cur-item))
    (setq is-convex (caddr cur-item)) (setq v-curr (cadddr cur-item))
    (setq is-active (nth 4 cur-item)) (setq is-open-end (nth 5 cur-item))
    (setq can-rotate (or is-convex is-open-end))
    (if (and is-active can-rotate)
      (progn
        (setq orig-dist (distance v-curr offset-pt))
        (setq prev-idx (if (= i 0) (1- num-verts) (1- i)))

        ;; 경계선 교차 검사를 위해 이전/다음 정점(Vertex) 추출
        (setq v-prev (if (= i 0) (if is-closed (nth (1- num-verts) vertices) nil) (nth (1- i) vertices)))
        (setq v-next (if (= i (1- num-verts)) (if is-closed (nth 0 vertices) nil) (nth (1+ i) vertices)))

        ;; 회피 각도 리스트 (45 -> 90 -> 30 -> 60 -> 15 -> 75)
        (setq evade-angles (list (/ pi 4.0) (/ pi 2.0) (/ pi 6.0) (/ pi 3.0) (/ pi 12.0) (* 5.0 (/ pi 12.0))))

        ;; 이전 모서리 검사
        (if (or is-closed (and (> i 0) (not is-closed)))
          (progn
            (setq prev-item (nth prev-idx base-data))
            (setq prev-active (nth 4 prev-item))
            (setq orig-angle-dir angle-dir)
            (setq test-idx 0)
            (while (and prev-active (< test-idx (length evade-angles))
                        (or (check-wedge-collision offset-pt angle-dir (car prev-item) (cadr prev-item) wedge-len wedge-half-ang s-factor)
                            (check-boundary-collision v-curr angle-dir v-prev v-next)))
              (setq ang-to-prev (angle offset-pt (car prev-item)))
              (setq diff (- ang-to-prev orig-angle-dir))
              (while (> diff pi) (setq diff (- diff (* 2.0 pi))))
              (while (< diff (- pi)) (setq diff (+ diff (* 2.0 pi))))
              (setq evade-sign (if (> diff 0) -1.0 1.0))
              (setq angle-dir (+ orig-angle-dir (* evade-sign (nth test-idx evade-angles))))
              (setq offset-pt (polar v-curr angle-dir orig-dist))
              (setq test-idx (1+ test-idx))
            )
          )
        )
        (setq next-idx (if (= i (1- num-verts)) 0 (1+ i)))
        (if (or is-closed (and (< i (1- num-verts)) (not is-closed)))
          (progn
            (setq next-item (nth next-idx base-data))
            (setq next-active (nth 4 next-item))
            (setq orig-angle-dir angle-dir)
            (setq test-idx 0)
            (while (and next-active (< test-idx (length evade-angles))
                        (or (check-wedge-collision offset-pt angle-dir (car next-item) (cadr next-item) wedge-len wedge-half-ang s-factor)
                            (check-boundary-collision v-curr angle-dir v-prev v-next)))
              (setq ang-to-next (angle offset-pt (car next-item)))
              (setq diff (- ang-to-next orig-angle-dir))
              (while (> diff pi) (setq diff (- diff (* 2.0 pi))))
              (while (< diff (- pi)) (setq diff (+ diff (* 2.0 pi))))
              (setq evade-sign (if (> diff 0) -1.0 1.0))
              (setq angle-dir (+ orig-angle-dir (* evade-sign (nth test-idx evade-angles))))
              (setq offset-pt (polar v-curr angle-dir orig-dist))
              (setq test-idx (1+ test-idx))
            )
          )
        )
      )
    )
    (setq final-data (append final-data (list (list offset-pt angle-dir v-curr is-active))))
    (setq i (1+ i))
  )
  ;; 심볼 테이블(레이어) 생성에 영향받지 않도록 각 객체 생성 직후 XDATA를 직접 부여하는 보조 함수
  (if (not (tblsearch "APPID" "TSP_CORNER_IDX")) (regapp "TSP_CORNER_IDX"))
  (defun tag-last-corner-ent (c-idx c-pt / e ed)
    (setq e (entlast))
    (if (and e (setq ed (entget e)))
      (entmod (append ed (list (list -3 (list "TSP_CORNER_IDX" (cons 1070 c-idx) (cons 1010 c-pt))))))
    )
  )
  (setq i 0)
  (while (< i num-verts)
    (setq cur-item (nth i final-data))
    (setq line-start (car cur-item)) (setq angle-dir (cadr cur-item))
    (setq v-curr (caddr cur-item)) (setq is-active (nth 3 cur-item))
    (if is-active
      (progn
        (setq angle-perp (+ angle-dir (/ pi 2.0)))
        (setq text-angle (- angle-dir (/ pi 2.0)))
        (setq text-val (1+ i))
        (setq cur-cum-count (cdr (assoc i cumulative-counts)))
        (if (and fully-closed (= i 0))
          (progn
            (setq mid-line-len (* 600.0 s-factor))
            (setq cap1-bot (polar line-start angle-dir line1-len))
            (setq cap1-center (polar cap1-bot angle-dir cap-h-half))
            (setq cap1-top (polar cap1-center angle-dir cap-h-half))
            (setq cap2-bot (polar cap1-top angle-dir mid-line-len))
            (setq cap2-center (polar cap2-bot angle-dir cap-h-half))
            (setq cap2-top (polar cap2-center angle-dir cap-h-half))
            (setq line2-end (polar cap2-top angle-dir line2-len))
            (setq circle-center (polar line2-end angle-dir circle-rad))
            (entmake (list '(0 . "LINE") (cons 8 layer-name) (cons 62 1) (cons 10 line-start) (cons 11 cap1-bot)))
            (tag-last-corner-ent i v-curr)
            (entmake (list '(0 . "LINE") (cons 8 layer-name) (cons 62 1) (cons 10 cap1-top) (cons 11 cap2-bot)))
            (tag-last-corner-ent i v-curr)
            (entmake (list '(0 . "LINE") (cons 8 layer-name) (cons 62 2) (cons 10 cap2-top) (cons 11 line2-end)))
            (tag-last-corner-ent i v-curr)
            (draw-capsule-shape cap1-center angle-dir angle-perp cap-h-half cap-w-half cap-rad layer-name 1)
            (tag-last-corner-ent i v-curr)
            (draw-capsule-shape cap2-center angle-dir angle-perp cap-h-half cap-w-half cap-rad layer-name 1)
            (tag-last-corner-ent i v-curr)
            (entmake (list '(0 . "CIRCLE") (cons 8 layer-name) (cons 62 2) (cons 10 circle-center) (cons 40 circle-rad)))
            (tag-last-corner-ent i v-curr)
            (entmake (list '(0 . "TEXT") (cons 8 layer-name) (cons 62 7) (cons 10 cap1-center) (cons 40 text-h-cnt) (cons 50 text-angle) (cons 1 (itoa total-count)) '(72 . 4) (cons 11 cap1-center)))
            (tag-last-corner-ent i v-curr)
            (entmake (list '(0 . "TEXT") (cons 8 layer-name) (cons 62 7) (cons 10 cap2-center) (cons 40 text-h-cnt) (cons 50 text-angle) (cons 1 (itoa cur-cum-count)) '(72 . 4) (cons 11 cap2-center)))
            (tag-last-corner-ent i v-curr)
            (entmake (list '(0 . "TEXT") (cons 8 layer-name) (cons 62 7) (cons 10 circle-center) (cons 40 text-h-num) (cons 50 text-angle) (cons 1 (itoa text-val)) '(72 . 4) (cons 11 circle-center)))
            (tag-last-corner-ent i v-curr)
          )
          (progn
            (setq cap-bot (polar line-start angle-dir line1-len))
            (setq cap-center (polar cap-bot angle-dir cap-h-half))
            (setq cap-top (polar cap-center angle-dir cap-h-half))
            (setq line2-end (polar cap-top angle-dir line2-len))
            (setq circle-center (polar line2-end angle-dir circle-rad))
            (entmake (list '(0 . "LINE") (cons 8 layer-name) (cons 62 1) (cons 10 line-start) (cons 11 cap-bot)))
            (tag-last-corner-ent i v-curr)
            (entmake (list '(0 . "LINE") (cons 8 layer-name) (cons 62 2) (cons 10 cap-top) (cons 11 line2-end)))
            (tag-last-corner-ent i v-curr)
            (draw-capsule-shape cap-center angle-dir angle-perp cap-h-half cap-w-half cap-rad layer-name 1)
            (tag-last-corner-ent i v-curr)
            (entmake (list '(0 . "CIRCLE") (cons 8 layer-name) (cons 62 2) (cons 10 circle-center) (cons 40 circle-rad)))
            (tag-last-corner-ent i v-curr)
            (entmake (list '(0 . "TEXT") (cons 8 layer-name) (cons 62 7) (cons 10 cap-center) (cons 40 text-h-cnt) (cons 50 text-angle) (cons 1 (itoa cur-cum-count)) '(72 . 4) (cons 11 cap-center)))
            (tag-last-corner-ent i v-curr)
            (entmake (list '(0 . "TEXT") (cons 8 layer-name) (cons 62 7) (cons 10 circle-center) (cons 40 text-h-num) (cons 50 text-angle) (cons 1 (itoa text-val)) '(72 . 4) (cons 11 circle-center)))
            (tag-last-corner-ent i v-curr)
          )
        )
      )
    )
    (setq i (1+ i))
  )
)

;;;;==========================================================================
;;;; defun tsp-calculate-brace-limit : 사보강재 한계거리(절대/권장) 산출
;;;;==========================================================================
(defun tsp-calculate-brace-limit (idx / is-closed num-segs prev-idx next-idx curr-seg prev-seg next-seg L1 L2 curr-brace prev-brace next-brace prev-used next-used margin1 margin2 avail1 avail2 hard-limit soft-limit curr-b prev-b next-b s-limit1 s-limit2)
  (setq num-segs (length *segment-list*))
  (setq curr-seg (nth idx *segment-list*))
  (setq L2 (/ (cdr (assoc 'LENGTH curr-seg)) 1000.0))
  ;; 현재 모서리(idx)의 사보강재 폭(B) 추출 (없으면 기본 300)
  (setq curr-brace (cdr (assoc 'CORNER-BRACE curr-seg)))
  (setq curr-b 300.0)
  (if (and curr-brace (= (car curr-brace) "Y") (cadr curr-brace))
    (setq curr-b (float (nth 1 (cadr (last (cadr curr-brace))))))
  )
  (if (and *tsp-boundary-ent* (is-closed-polyline *tsp-boundary-ent*))
    (setq is-closed T)
    (setq is-closed nil)
  )
  (setq prev-idx (if (= idx 0) (if is-closed (1- num-segs) -1) (1- idx)))
  (setq next-idx (if (= idx (1- num-segs)) (if is-closed 0 -1) (1+ idx)))
  ;; 1. 이전 선분(L1) 가용성 검사
  (if (>= prev-idx 0)
    (progn
      (setq prev-seg (nth prev-idx *segment-list*))
      (setq L1 (/ (cdr (assoc 'LENGTH prev-seg)) 1000.0))
      (setq prev-brace (cdr (assoc 'CORNER-BRACE prev-seg)))
      (setq prev-b 300.0 prev-used 0.0)
      (if (and prev-brace (= (car prev-brace) "Y"))
        (if (cadr prev-brace)
          (progn
            (setq prev-used (nth 2 (last (cadr prev-brace))))
            (setq prev-b (float (nth 1 (cadr (last (cadr prev-brace))))))
          )
          ;; 미입력 상태: 무조건 최소 1.0m만 예약 (유연한 배분)
          (setq prev-used 1.0)
        )
      )
      ;; [동적 마진 계산] (나의 B/2 + 이웃의 B/2) * 1.414(45도 빗변) + 화타쐐기 여유 150mm
      (setq margin1 (+ (* (+ (/ curr-b 2000.0) (/ prev-b 2000.0)) 1.414) 0.15))
      (setq avail1 (- L1 prev-used margin1))
    )
    (progn (setq L1 999.0) (setq avail1 999.0) (setq prev-brace nil))
  )
  ;; 2. 현재 선분(L2) 가용성 검사
  (if (>= next-idx 0)
    (progn
      (setq next-seg (nth next-idx *segment-list*))
      (setq next-brace (cdr (assoc 'CORNER-BRACE next-seg)))
      (setq next-b 300.0 next-used 0.0)
      (if (and next-brace (= (car next-brace) "Y"))
        (if (cadr next-brace)
          (progn
            (setq next-used (nth 2 (last (cadr next-brace))))
            (setq next-b (float (nth 1 (cadr (last (cadr next-brace))))))
          )
          ;; 미입력 상태: 최소 1.0m 예약
          (setq next-used 1.0)
        )
      )
      (setq margin2 (+ (* (+ (/ curr-b 2000.0) (/ next-b 2000.0)) 1.414) 0.15))
      (setq avail2 (- L2 next-used margin2))
    )
    (progn (setq L2 999.0) (setq avail2 999.0) (setq next-brace nil))
  )
  ;; 3. 최종 이중 한계치 도출 (List 형태로 반환)
  (setq hard-limit (min avail1 avail2))
  (if (< hard-limit 0.0) (setq hard-limit 0.0))
  ;; 이웃 모서리에 사보강재가 없을 경우 50% 제한을 해제하고 해당 방향의 물리적 한계치 적용
  (setq s-limit1 (if (and prev-brace (= (car prev-brace) "Y")) (/ L1 2.0) avail1))
  (setq s-limit2 (if (and next-brace (= (car next-brace) "Y")) (/ L2 2.0) avail2))
  (setq soft-limit (min s-limit1 s-limit2))
  ;; 만약 권장 한계(soft-limit)가 계산상 절대 한계(hard-limit)보다 커진다면 절대 한계로 하향 고정
  (if (> soft-limit hard-limit) (setq soft-limit hard-limit))
  (list hard-limit soft-limit)
)
