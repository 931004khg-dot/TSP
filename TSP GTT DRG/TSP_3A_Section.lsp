;;; ==========================================================================
;;; 파일명: TSP_3A_Section.lsp
;;; 구  분: 단면도 및 지반 주상도 작도 (Part A)
;;; 역  할: 단면 띠장/유압잭/스크류잭 블록 정의, 지하수위 기호, 지반 주상도 및 N치 작도, H-Pile+토류판/CIP 수직 단면도, 단면 지보재/브래킷/숏크리트/그라우팅 작도 [create-wale-section-block, draw-water-level-symbol, draw-soil-column, draw-wale-section, draw-vertical-hpile]
;;; 포함 함수: create-wale-section-block, draw-water-level-symbol, draw-soil-column, draw-wale-section, draw-vertical-hpile
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;; ==========================================================================
;;; [SECTION 4] 단면도 및 주상도 작도 (Section & Soil Profile)
;;; ==========================================================================

;;;;==========================================================================
;;;; defun create-wale-section-block : 단면 띠장(Wale) 블록 정의
;;;;==========================================================================
(defun create-wale-section-block (h b tw tf / block-name pts layer-name)
    ;; 블록 이름과 레이어 이름 생성
    (setq block-name (strcat "_WALE(" (itoa h) "x" (itoa b) "x" (itoa tw) "x" (itoa tf) ")"))
    (setq layer-name (strcat "_H-Pile_" (itoa h) "x" (itoa b) "x" (itoa tw) "-" (itoa tf)))
    (if (or (null layer-name) (= layer-name "")) (setq layer-name "0"))
    ;; 블록이 없으면 생성
    (if (not (tblsearch "BLOCK" block-name))
      (progn
        ;; 필요한 레이어 생성
        (create-layer-if-not-exists layer-name "3")
        (create-layer-if-not-exists "_띠장 point" "1")
        (create-layer-if-not-exists "_띠장(wale)" "3")
        (entmake (list '(0 . "BLOCK") (cons 2 block-name) '(70 . 0) '(10 . (0.0 0.0 0.0))))
        (setq pts (calc-hpile-vertices (list 0.0 (/ h 2.0) 0.0) h b tw tf))
        ;; 1. H-Pile 형상 작도
        (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
          (cons 8 layer-name) (cons 62 256) '(90 . 20) '(70 . 1)
          (cons 10 (nth 0 pts))  (cons 10 (nth 1 pts))
          (cons 10 (nth 2 pts))  (cons 42 0.4142135623730951) (cons 10 (nth 3 pts))
          (cons 10 (nth 4 pts))  (cons 42 0.4142135623730951) (cons 10 (nth 5 pts))
          (cons 10 (nth 6 pts))  (cons 10 (nth 7 pts))
          (cons 10 (nth 8 pts))  (cons 10 (nth 9 pts))
          (cons 10 (nth 10 pts)) (cons 42 0.4142135623730951) (cons 10 (nth 11 pts))
          (cons 10 (nth 12 pts)) (cons 42 0.4142135623730951) (cons 10 (nth 13 pts))
          (cons 10 (nth 14 pts)) (cons 10 (nth 15 pts))))
        ;; 2. STIFFNER_외곽 모서리 직사각형 (pt1-pt7-pt12-pt6)
        (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
          (cons 8 "_띠장(wale)") (cons 62 256) '(90 . 4) '(70 . 1)
          (cons 10 (nth 0 pts))   ;; pt1 (우상단)
          (cons 10 (nth 7 pts))   ;; pt7 (우하단)
          (cons 10 (nth 8 pts))   ;; pt12 (좌하단)
          (cons 10 (nth 15 pts))  ;; pt6 (좌상단)
        ))
        (entmake (list '(0 . "POINT") (cons 8 "_띠장 point") (cons 62 256) '(10 0.0 0.0 0.0)))
        (entmake (list '(0 . "ENDBLK")))
      )
    )
    block-name
  )

;;;;==========================================================================
;;;; defun draw-water-level-symbol : 지하수위 기호(삼각형 역상) 및 수위 텍스트 작도
;;;;==========================================================================
(defun draw-water-level-symbol (base-pt depth-val col-x-right / layer-name line1-mid line1-start line1-end line2-start line2-end line3-start line3-end tri-p1 tri-p2 tri-p3 text-pt text-str s-factor tri-h tri-w-half line1-len l2-half off-y1 line2-y l3-half off-y2 line3-y tri-p_top_center text-gap text-h)
  (setq layer-name "_지반선")
  (setq s-factor (/ *tsp-scale-section* 200.0))
  (setq tri-h (* 800.0 s-factor))
  (setq tri-w-half (/ tri-h (sqrt 3.0)))
  (setq line1-len (* 3000.0 s-factor))
  (setq line1-start (list col-x-right (cadr base-pt) 0.0))
  (setq line1-end (list (+ col-x-right line1-len) (cadr base-pt) 0.0))
  (setq line1-mid (list (/ (+ (car line1-start) (car line1-end)) 2.0) (cadr base-pt) 0.0))
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 2) '(70 . 0) (cons 10 line1-start) (cons 10 line1-end)))
  (setq l2-half (* 750.0 s-factor)) (setq off-y1 (* 100.0 s-factor)) (setq line2-y (- (cadr line1-mid) off-y1))
  (setq line2-start (list (- (car line1-mid) l2-half) line2-y 0.0)) (setq line2-end (list (+ (car line1-mid) l2-half) line2-y 0.0))
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 2) '(70 . 0) (cons 10 line2-start) (cons 10 line2-end)))
  (setq l3-half (* 500.0 s-factor)) (setq off-y2 (* 200.0 s-factor)) (setq line3-y (- (cadr line1-mid) off-y2))
  (setq line3-start (list (- (car line1-mid) l3-half) line3-y 0.0)) (setq line3-end (list (+ (car line1-mid) l3-half) line3-y 0.0))
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 2) '(70 . 0) (cons 10 line3-start) (cons 10 line3-end)))
  (setq tri-p3 line1-mid)
  (setq tri-p1 (list (- (car line1-mid) tri-w-half) (+ (cadr line1-mid) tri-h) 0.0))
  (setq tri-p2 (list (+ (car line1-mid) tri-w-half) (+ (cadr line1-mid) tri-h) 0.0))
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 3) '(70 . 1) (cons 10 tri-p1) (cons 10 tri-p2) (cons 10 tri-p3)))
  (setq tri-p_top_center (list (car line1-mid) (+ (cadr line1-mid) tri-h) 0.0))
  (entmake (list '(0 . "SOLID") '(100 . "AcDbEntity") '(100 . "AcDbTrace") (cons 8 layer-name) (cons 62 8) (cons 10 tri-p1) (cons 11 tri-p_top_center) (cons 12 tri-p3) (cons 13 tri-p3)))
  (setq text-gap (* 400.0 s-factor))
  (setq text-str (strcat "-" (format-depth-text depth-val) "m"))
  (setq text-pt (list (car line1-mid) (+ (cadr line1-mid) tri-h text-gap) 0.0))
  (setq text-h (* 600.0 s-factor))
  (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText") (cons 8 layer-name) (cons 62 8) (cons 10 text-pt) (cons 40 text-h) (cons 1 text-str) (cons 72 1) (cons 11 text-pt)))
)

;;;;==========================================================================
;;;; defun draw-soil-column : 지반 주상도(Soil Column) 생성 및 지층별 해치/텍스트 표기
;;;;==========================================================================
(defun draw-soil-column (top-left-pt side / prev-depth curr-depth layer-name layer-depth layer-hatch layer-scale layer-angle col-width y-curr y-prev rect-pl start-x s-factor txt-h leader-len txt-off-l txt-off-c col-x-right pt1 pt2 pt3 pt4 wl-y wl-pt l-name l-depth l-pattern l-scale l-angle text-x text-y-name center-pt base-scale final-scale final-angle hole-text-x hole-text-y n-item n-depth n-str n-y n-pt1 n-pt2 n-text-pt)
  (setq layer-name "_지반선")
  (setq s-factor (/ *tsp-scale-section* 200.0))
  (setq col-width (* 1000.0 s-factor)) (setq txt-h (* 600.0 s-factor)) (setq leader-len (* 1000.0 s-factor)) (setq txt-off-l (* 1200.0 s-factor)) (setq txt-off-c (* 3500.0 s-factor))
  (setq prev-depth 0.0)
  (if (= side "L") (setq start-x (- (car top-left-pt) col-width)) (setq start-x (car top-left-pt)))
  (setq top-left-pt (list start-x (cadr top-left-pt) 0.0))
  (setq col-x-right (+ start-x col-width))
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 2) '(70 . 0) (cons 10 (list (car top-left-pt) (cadr top-left-pt))) (cons 10 (list (- (car top-left-pt) leader-len) (cadr top-left-pt)))))
  (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText") (cons 8 layer-name) (cons 62 8) (cons 10 (list (- (car top-left-pt) txt-off-l) (cadr top-left-pt) 0.0)) (cons 40 txt-h) (cons 1 (format-depth-text 0.0)) (cons 72 2) (cons 11 (list (- (car top-left-pt) txt-off-l) (cadr top-left-pt) 0.0))))
  (if (= *tsp-water-chk* "1")
    (progn
      (setq wl-y (- (cadr top-left-pt) (* *tsp-water-depth* 1000.0)))
      (setq wl-x (+ col-x-right (* 3000.0 s-factor)))
      (setq wl-pt (list wl-x wl-y 0.0))
      (draw-water-level-symbol wl-pt *tsp-water-depth* wl-x)
    )
  )
  (foreach layer *tsp-soil-layers*
    (setq l-name (car layer)) (setq l-depth (cadr layer)) (setq l-pattern (nth 2 layer)) (setq l-scale (nth 3 layer)) (setq l-angle (nth 4 layer))
    (setq curr-depth (* l-depth 1000.0)) (setq y-prev (- (cadr top-left-pt) prev-depth)) (setq y-curr (- (cadr top-left-pt) curr-depth))
    (setq pt1 (list (car top-left-pt) y-prev)) (setq pt2 (list (+ (car top-left-pt) col-width) y-prev)) (setq pt3 (list (+ (car top-left-pt) col-width) y-curr)) (setq pt4 (list (car top-left-pt) y-curr))
    (setq center-pt (list (/ (+ (car pt1) (car pt3)) 2.0) (/ (+ (cadr pt1) (cadr pt3)) 2.0) 0.0))
    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 4) '(70 . 1) (cons 10 pt1) (cons 10 pt2) (cons 10 pt3) (cons 10 pt4)))
    (setq rect-pl (entlast))
    ;; 배열에 저장된 l-scale과 l-angle 적용
    (setq base-scale (if (and l-scale (> l-scale 0)) l-scale 100.0))
    (setq final-scale (* base-scale (/ *tsp-scale-section* 200.0)))
    (setq final-angle (if l-angle l-angle 0.0))
    (if rect-pl
      (tsp-apply-custom-hatch rect-pl l-pattern final-scale (* final-angle (/ pi 180.0)) layer-name 8 center-pt)
    )
    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 2) '(70 . 0) (cons 10 pt4) (cons 10 (list (- (car pt4) leader-len) (cadr pt4)))))
    (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText") (cons 8 layer-name) (cons 62 8) (cons 10 (list (- (car pt4) txt-off-l) (cadr pt4) 0.0)) (cons 40 txt-h) (cons 1 (format-depth-text l-depth)) (cons 72 2) (cons 11 (list (- (car pt4) txt-off-l) (cadr pt4) 0.0))))
    (setq text-x (- (+ (car pt1) (/ col-width 2.0)) txt-off-c))
    (setq text-y-name (- (/ (+ y-prev y-curr) 2.0) (/ txt-h 2.0)))
    (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText") (cons 8 layer-name) (cons 62 8) (cons 10 (list text-x text-y-name 0.0)) (cons 40 txt-h) (cons 1 l-name) (cons 72 2) (cons 11 (list text-x text-y-name 0.0))))
    (setq prev-depth curr-depth)
  )
;;;;==========================================================================
;;;; N치 및 시추공 번호(HOLE No.) 우측 작도 로직
;;;;==========================================================================
  (if (and *tsp-hole-no* (/= *tsp-hole-no* ""))
    (progn
      (setq h-txt-h (* 500.0 s-factor))
      (setq center-top-x (+ start-x (/ col-width 2.0)))
      (setq center-top-y (cadr top-left-pt))
      (setq line-top-y (+ center-top-y (* 1000.0 s-factor)))

      ;; 1. 중심 수직선 (길이 1000)
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 2) '(70 . 0) (cons 10 (list center-top-x center-top-y)) (cons 10 (list center-top-x line-top-y))))

      ;; 2. 직사각형 박스 (가로 2200, 세로 800)
      (setq box-hw (* 1100.0 s-factor))
      (setq box-h (* 800.0 s-factor))
      (setq p1 (list (- center-top-x box-hw) line-top-y))
      (setq p2 (list (+ center-top-x box-hw) line-top-y))
      (setq p3 (list (+ center-top-x box-hw) (+ line-top-y box-h)))
      (setq p4 (list (- center-top-x box-hw) (+ line-top-y box-h)))
      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 4) '(70 . 1) (cons 10 p1) (cons 10 p2) (cons 10 p3) (cons 10 p4)))

      ;; 3. 시추공 번호 텍스트 (박스 중앙 정렬)
      (setq text-y (+ line-top-y (* 400.0 s-factor)))
      (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText")
                     (cons 8 layer-name) (cons 62 8)
                     (cons 10 (list center-top-x text-y 0.0))
                     (cons 40 h-txt-h)
                     (cons 1 *tsp-hole-no*)
                     (cons 72 1) (cons 11 (list center-top-x text-y 0.0)) (cons 73 2)))
    )
  )

  (if *tsp-n-values*
    (progn
      (setq leader-len (* 2000.0 s-factor))
      (foreach n-item *tsp-n-values*
        (setq n-depth (* (car n-item) 1000.0))
        (setq n-str (cadr n-item))
        (setq n-y (- (cadr top-left-pt) n-depth))

        (setq n-pt1 (list col-x-right n-y))
        (setq n-pt2 (list (+ col-x-right leader-len) n-y))

        ;; 지시선 그리기
        (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 2) '(70 . 0) (cons 10 n-pt1) (cons 10 n-pt2)))

        ;; N치 텍스트 (지시선 시작점 n-pt1에서 오른쪽으로 100만큼 여백을 두고 배치)
        (setq n-text-pt (list (+ (car n-pt1) (* 100.0 s-factor)) (+ n-y (* 150.0 s-factor)) 0.0))
        (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText")
                       (cons 8 layer-name) (cons 62 8)
                       (cons 10 n-text-pt)
                       (cons 40 txt-h)
                       (cons 1 n-str)))
      )
    )
  )
  (princ "\n주상도 생성 완료.")
)

;;;;==========================================================================
;;;; defun draw-wale-section : 띠장 삽입 및 보걸이 상세 작도
;;;;==========================================================================
(defun draw-wale-section (insert-pt rotation layer-name w-h w-b w-tw w-tf extra-margin br-offset / block-name br-layer cx cy wale-dir y-base y-L1 y-L2 y-L3 x-wall x-tip x-L4-start pt-L1-s pt-L1-e pt-L2-s pt-L2-e pt-L3-s pt-L3-e pt-L4-s pt-L4-e pt-L5-s pt-L5-e pt-L6-s pt-L6-e pt-L7-s pt-L7-e pt-L8-s pt-L8-e k4 k6 k7 is-left trim-x-6 trim-x-7 intersect-x8)
  (setq block-name (create-wale-section-block w-h w-b w-tw w-tf))
  (entmake (list '(0 . "INSERT") (cons 2 block-name) (cons 8 layer-name) (cons 10 insert-pt) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 rotation)))
  (setq br-layer "_보걸이(Angle Bracket)")
  (create-layer-if-not-exists br-layer "7")
  (setq cx (car insert-pt) cy (cadr insert-pt))
  (if (< (sin rotation) -0.1) (setq wale-dir 1.0) (setq wale-dir -1.0))
  (setq is-left (> wale-dir 0))
  (setq y-base (- cy (/ w-b 2.0)))
  (setq y-L1 y-base)
  (setq y-L2 (- y-base 10.0))
  (setq y-L3 (- y-base 100.0))
  (setq x-wall (- cx (* (if br-offset br-offset 0.0) wale-dir)))
  (setq x-tip (+ cx (* (+ w-h extra-margin) wale-dir)))
  (setq pt-L1-s (list x-wall y-L1 0.0)) (setq pt-L1-e (list x-tip y-L1 0.0))
  (setq pt-L2-s (list x-wall y-L2 0.0)) (setq pt-L2-e (list x-tip y-L2 0.0))
  (setq pt-L3-s pt-L1-e) (setq pt-L3-e (list (car pt-L1-e) y-L3 0.0))
  (setq x-L4-start (- x-tip (* 30.0 wale-dir)))
  (setq pt-L4-s (list x-L4-start y-L2 0.0))
  (if is-left (setq k4 (- y-L2 x-L4-start)) (setq k4 (+ y-L2 x-L4-start)))
  (if is-left (setq pt-L4-e (list x-wall (+ x-wall k4) 0.0)) (setq pt-L4-e (list x-wall (- k4 x-wall) 0.0)))
  (setq pt-L5-s pt-L3-e)
  (if is-left (setq pt-L5-e (list (- y-L3 k4) y-L3 0.0)) (setq pt-L5-e (list (- k4 y-L3) y-L3 0.0)))
  (setq k6 (+ k4 (* 90.0 (sqrt 2.0))))
  (if is-left (setq trim-x-6 (- y-L2 k6)) (setq trim-x-6 (- k6 y-L2)))
  (setq pt-L6-s (list trim-x-6 y-L2 0.0)) (setq pt-L6-e (list x-wall (if is-left (+ x-wall k6) (- k6 x-wall)) 0.0))
  (setq k7 (+ k6 (* 10.0 (sqrt 2.0))))
  (if is-left (setq trim-x-7 (- y-L2 k7)) (setq trim-x-7 (- k7 y-L2)))
  (setq pt-L7-s (list trim-x-7 y-L2 0.0)) (setq pt-L7-e (list x-wall (if is-left (+ x-wall k7) (- k7 x-wall)) 0.0))
  (if is-left (setq intersect-x8 (- y-L3 k7)) (setq intersect-x8 (- k7 y-L3)))
  (setq pt-L8-s (list intersect-x8 y-L3 0.0))
  (setq pt-L8-e (list x-wall y-L3 0.0))
  (foreach pts (list (list pt-L1-s pt-L1-e) (list pt-L3-s pt-L3-e) (list pt-L4-s pt-L4-e)
                     (list pt-L5-s pt-L5-e) (list pt-L7-s pt-L7-e))
    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
      (cons 8 br-layer) (cons 62 256) '(90 . 2) '(70 . 0) (cons 10 (car pts)) (cons 10 (cadr pts)))))
  ;; 교차점(intersect-x8)이 벽체(x-wall) 안쪽에 위치할 때 보걸이 하단 안쪽 수평선(L8) 작도
  (if (if is-left (> intersect-x8 x-wall) (< intersect-x8 x-wall))
    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
      (cons 8 br-layer) (cons 62 256) '(90 . 2) '(70 . 0) (cons 10 pt-L8-s) (cons 10 pt-L8-e)))
  )
  (foreach pts (list (list pt-L2-s pt-L2-e) (list pt-L6-s pt-L6-e))
    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
      (cons 8 br-layer) (cons 62 1) '(90 . 2) '(70 . 0) (cons 10 (car pts)) (cons 10 (cadr pts)))))
)

;;;;==========================================================================
;;;; defun draw-vertical-hpile : H-Pile 수직 단면선 작도
;;;;==========================================================================
(defun draw-vertical-hpile (x-left x-right y-top y-bot tf layer-name / x-in-l x-in-r p-out-l-top p-out-l-bot p-in-l-top p-in-l-bot p-in-r-top p-in-r-bot p-out-r-top p-out-r-bot)
  (setq x-in-l (+ x-left tf))
  (setq x-in-r (- x-right tf))

  (setq p-out-l-top (list x-left y-top 0.0))
  (setq p-out-l-bot (list x-left y-bot 0.0))
  (setq p-in-l-top (list x-in-l y-top 0.0))
  (setq p-in-l-bot (list x-in-l y-bot 0.0))
  (setq p-in-r-top (list x-in-r y-top 0.0))
  (setq p-in-r-bot (list x-in-r y-bot 0.0))
  (setq p-out-r-top (list x-right y-top 0.0))
  (setq p-out-r-bot (list x-right y-bot 0.0))

  ;; 좌측 외곽선 (초록색, 3)
  (entmake (list '(0 . "LINE") '(100 . "AcDbEntity") '(100 . "AcDbLine") (cons 8 layer-name) (cons 62 3) (cons 10 p-out-l-top) (cons 11 p-out-l-bot)))
  ;; 좌측 내부 두께선 (빨간색, 1)
  (entmake (list '(0 . "LINE") '(100 . "AcDbEntity") '(100 . "AcDbLine") (cons 8 layer-name) (cons 62 1) (cons 10 p-in-l-top) (cons 11 p-in-l-bot)))
  ;; 우측 내부 두께선 (빨간색, 1)
  (entmake (list '(0 . "LINE") '(100 . "AcDbEntity") '(100 . "AcDbLine") (cons 8 layer-name) (cons 62 1) (cons 10 p-in-r-top) (cons 11 p-in-r-bot)))
  ;; 우측 외곽선 (초록색, 3)
  (entmake (list '(0 . "LINE") '(100 . "AcDbEntity") '(100 . "AcDbLine") (cons 8 layer-name) (cons 62 3) (cons 10 p-out-r-top) (cons 11 p-out-r-bot)))
  ;; 상단 닫힘 선 (초록색, 3)
  (entmake (list '(0 . "LINE") '(100 . "AcDbEntity") '(100 . "AcDbLine") (cons 8 layer-name) (cons 62 3) (cons 10 p-out-l-top) (cons 11 p-out-r-top)))
  ;; 하단 닫힘 선 (초록색, 3)
  (entmake (list '(0 . "LINE") '(100 . "AcDbEntity") '(100 . "AcDbLine") (cons 8 layer-name) (cons 62 3) (cons 10 p-out-l-bot) (cons 11 p-out-r-bot)))
)
