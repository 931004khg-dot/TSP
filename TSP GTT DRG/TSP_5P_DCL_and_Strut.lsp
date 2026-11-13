;;; ==========================================================================
;;; 파일명: TSP_5P_DCL_and_Strut.lsp
;;; 구  분: DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part P)
;;; 역  할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-draw-realtime-continuous-braces]
;;; 포함 함수: tsp-draw-realtime-continuous-braces
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-draw-realtime-continuous-braces : 순수 좌표 추적 기반 연속 보강재 실시간 분할 작도 함수
;;;;   - 기울어진 버팀보 그룹에서도 보강재가 항상 버팀보에 수직(90°)인 일직선으로 전체 그룹을 관통하도록 작도
;;;;==========================================================================
(defun tsp-draw-realtime-continuous-braces (pts-list s-brace-opt sec-str is-double strut-ang strut-len seg-id / boundary-handle stiff-enable slender-limit is-manual manual-interval margin strut-vals h b tw tf half-b half-tw strut-sys-half-width brace-xdata mid-gap-xdata brace-ents total-L L-max offsets interval num-div i draw-segs mid-gap-segs num-pts idx cur-pt p-center p-left p-right p-start p-mid-left p-mid-right next-pt p-next-center p-next-left p-end pA pB f1-1 f1-2 f2-1 f2-2 w1-1 w1-2 w2-1 w2-2 base-ref-pt ref-len min-clear k-grid u-grid active-pts pt-len rec off-local next-off num-draw-segs seg-idx seg-ang)
  (setq stiff-enable (nth 0 s-brace-opt) slender-limit (nth 1 s-brace-opt) is-manual (nth 2 s-brace-opt) manual-interval (nth 3 s-brace-opt) margin (atof (nth 4 s-brace-opt)))
  (if (/= stiff-enable "1") (exit))
  (setq boundary-handle (tsp-get-entity-handle *tsp-boundary-ent*))

  (create-layer-if-not-exists "_보강재(Brace)" "7")
  (if (not (tblsearch "LTYPE" "DASHED")) (vl-catch-all-apply 'vla-load (list (vla-get-linetypes (vla-get-activedocument (vlax-get-acad-object))) "DASHED" (if (= (getvar "MEASUREMENT") 1) "acadiso.lin" "acad.lin"))))

  ;; 1. 제원 파싱 및 단면 치수 정의
  (setq strut-vals (parse-h-spec (if (wcmatch sec-str "2H *") (vl-string-subst "H " "2H " sec-str) sec-str)))
  (if (null strut-vals) (setq strut-vals '(300 300 10 15)))
  (setq h (float (nth 0 strut-vals)) b (float (nth 1 strut-vals)) tw (float (nth 2 strut-vals)) tf (float (nth 3 strut-vals)))
  (setq half-b (/ b 2.0) half-tw (/ tw 2.0))
  (setq strut-sys-half-width (if is-double (* b 1.5) (/ b 2.0)))

  ;; ARX 크래시 방지를 위해 LISP 전용 APPID 부여
  (if (not (tblsearch "APPID" "TSP_LISP_BRACE")) (regapp "TSP_LISP_BRACE"))
  (setq brace-xdata
    (list -3
      (list "TSP_LISP_BRACE"
        (cons 1070 seg-id)
        (cons 1040 0.0)
        (cons 1010 '(0.0 0.0 0.0))
        (cons 1010 '(0.0 0.0 0.0))
        (cons 1040 (float h))
        (cons 1040 (float tw))
        (cons 1040 (float tf))
        (cons 1000 "CONT_BRACE")
        (cons 1000 boundary-handle)
      )
    )
  )
  ;; 맞버팀대 중앙 간극 선분 전용 XDATA: MID_GAP 태그로 수동추가 교차점 검색에서 제외
  (setq mid-gap-xdata
    (list -3
      (list "TSP_LISP_BRACE"
        (cons 1070 seg-id)
        (cons 1040 0.0)
        (cons 1010 '(0.0 0.0 0.0))
        (cons 1010 '(0.0 0.0 0.0))
        (cons 1040 (float h))
        (cons 1040 (float tw))
        (cons 1040 (float tf))
        (cons 1000 "MID_GAP")
        (cons 1000 boundary-handle)
      )
    )
  )

  (setq brace-ents '())
  (if (> (length pts-list) 0)
    (progn
      ;; 그룹 기준 시작점(base-ref-pt) 및 기준 길이(ref-len) 확보 (중간말뚝 배치와 100% 동일한 격자 기준)
      (setq base-ref-pt (if (and (boundp '*tsp-1h-jack-base*) *tsp-1h-jack-base*) *tsp-1h-jack-base* (car pts-list)))
      (setq ref-len (if (and (boundp '*tsp-group-ref-len*) (numberp *tsp-group-ref-len*) (> *tsp-group-ref-len* 0.0)) *tsp-group-ref-len* strut-len))

      ;; 2. 실시간 입력된 순수 좌표들을 버팀보 직교 축(+90°) 방향으로 기하학적 정렬
      (setq pts-list (vl-sort pts-list
        '(lambda (p1 p2)
           (< (tsp-project-dist base-ref-pt (+ strut-ang (/ pi 2.0)) p1)
              (tsp-project-dist base-ref-pt (+ strut-ang (/ pi 2.0)) p2))
         )
      ))

      ;; 3. 세장비 한계에 따른 등분선 간격(interval) 및 격자 좌표(u-grid) 산출
      (setq total-L (/ ref-len 1000.0))
      (setq L-max (tsp-calc-unsupported-length strut-vals (if (= slender-limit "0") 100.0 120.0)))
      (setq offsets '())

      (if (or (> total-L L-max) (= is-manual "1"))
        (progn
          (if (= is-manual "1")
            (setq interval (* (atof manual-interval) 1000.0))
            (progn
              (setq num-div (fix (+ (/ total-L L-max) 0.9999)))
              (if (< num-div 1) (setq num-div 1))
              (setq interval (/ ref-len num-div))
            )
          )
          (if (> interval 10.0)
            (progn
              (setq min-clear (min 650.0 (* interval 0.15)))
              (setq k-grid -30)
              (while (<= k-grid 50)
                (setq offsets (append offsets (list (* k-grid interval))))
                (setq k-grid (1+ k-grid))
              )
            )
          )
        )
      )

      ;; 4. 각 직교 등분선(u-grid)마다 실제로 교차하는 버팀보들을 모아 버팀보에 수직(90°)인 일직선 관통 보강재 작도
      (foreach u-grid offsets
        (setq active-pts '())
        (foreach cur-pt pts-list
          (setq pt-len strut-len)
          (if (and (boundp '*tsp-strut-len-map*) *tsp-strut-len-map*)
            (foreach rec *tsp-strut-len-map*
              (if (< (distance (list (car cur-pt) (cadr cur-pt) 0.0) (car rec)) 10.0)
                (setq pt-len (cdr rec))
              )
            )
          )
          ;; 각 버팀보별 까치발 이격거리(*tsp-strut-clear-map*)를 조회하여 까치발과 겹치는 보강재 세그먼트는 생성 제외
          (setq pt-clear-1 min-clear pt-clear-2 min-clear)
          (if (and (boundp '*tsp-strut-clear-map*) *tsp-strut-clear-map*)
            (foreach c-rec *tsp-strut-clear-map*
              (if (< (distance (list (car cur-pt) (cadr cur-pt) 0.0) (car c-rec)) 10.0)
                (setq pt-clear-1 (car (cdr c-rec)) pt-clear-2 (cdr (cdr c-rec)))
              )
            )
          )
          (setq off-local (- u-grid (tsp-project-dist base-ref-pt strut-ang cur-pt)))
          (if (and (> off-local pt-clear-1) (< off-local (- pt-len pt-clear-2)))
            (setq active-pts (append active-pts (list cur-pt)))
          )
        )

        (if (> (length active-pts) 0)
          (progn
            (setq draw-segs '())
            (setq mid-gap-segs '())
            (setq num-pts (length active-pts))
            (setq idx 0)

            (while (< idx num-pts)
              (setq cur-pt (nth idx active-pts))
              (setq off-local (- u-grid (tsp-project-dist base-ref-pt strut-ang cur-pt)))
              (setq p-center (polar cur-pt strut-ang off-local))
              (setq p-left (polar p-center (- strut-ang (/ pi 2.0)) strut-sys-half-width))
              (setq p-right (polar p-center (+ strut-ang (/ pi 2.0)) strut-sys-half-width))

              ;; 4-1. 해당 직교 보강재 라인의 맨 첫 번째(idx=0) 왼쪽 끝 여유장 구간 배치
              (if (= idx 0)
                (progn
                  (setq p-start (polar p-left (- strut-ang (/ pi 2.0)) margin))
                  (setq draw-segs (append draw-segs (list (cons p-start p-left))))
                )
              )

              ;; 4-2. 맞버팀대(2H) 내부 중앙 간극 공간 자동 트림 복원
              (if is-double
                (progn
                  (setq p-mid-left (polar p-center (- strut-ang (/ pi 2.0)) half-b))
                  (setq p-mid-right (polar p-center (+ strut-ang (/ pi 2.0)) half-b))
                  (setq mid-gap-segs (append mid-gap-segs (list (cons p-mid-left p-mid-right))))
                )
              )

              ;; 4-3. 현재 버팀보 우측 경계면에서 다음 버팀보 좌측 경계면까지 수직(90°) 일직선 연결
              (if (< idx (1- num-pts))
                (progn
                  (setq next-pt (nth (1+ idx) active-pts))
                  (setq next-off (- u-grid (tsp-project-dist base-ref-pt strut-ang next-pt)))
                  (setq p-next-center (polar next-pt strut-ang next-off))
                  (setq p-next-left (polar p-next-center (- strut-ang (/ pi 2.0)) strut-sys-half-width))
                  (setq draw-segs (append draw-segs (list (cons p-right p-next-left))))
                )
              )

              ;; 4-4. 해당 직교 보강재 라인의 맨 마지막 우측 끝 여유장 구간 배치
              (if (= idx (1- num-pts))
                (progn
                  (setq p-end (polar p-right (+ strut-ang (/ pi 2.0)) margin))
                  (setq draw-segs (append draw-segs (list (cons p-right p-end))))
                )
              )

              (setq idx (1+ idx))
            )

            ;; 5. 가상 트림이 완료된 빈 공간 세그먼트들에만 물리 객체 작도 (XDATA 결합)
            (setq num-draw-segs (length draw-segs))
            (setq seg-idx 0)
            (foreach seg draw-segs
              (setq pA (car seg) pB (cdr seg))
              (if (> (distance pA pB) 0.1)
                (progn
                  (setq seg-ang (angle pA pB))
                  ;; 플랜지 선 1 (pA~pB의 +half-b 방향, 보강재 진행방향 수직)
                  (setq f1-1 (polar pA (+ seg-ang (/ pi 2.0)) half-b))
                  (setq f1-2 (polar pB (+ seg-ang (/ pi 2.0)) half-b))
                  (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 256) '(90 . 2) '(70 . 0)
                                 (list 10 (car f1-1) (cadr f1-1)) (list 10 (car f1-2) (cadr f1-2)))
                                 (list brace-xdata)))
                  (setq brace-ents (append brace-ents (list (entlast))))
                  ;; 플랜지 선 2 (pA~pB의 -half-b 방향)
                  (setq f2-1 (polar pA (- seg-ang (/ pi 2.0)) half-b))
                  (setq f2-2 (polar pB (- seg-ang (/ pi 2.0)) half-b))
                  (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 256) '(90 . 2) '(70 . 0)
                                 (list 10 (car f2-1) (cadr f2-1)) (list 10 (car f2-2) (cadr f2-2)))
                                 (list brace-xdata)))
                  (setq brace-ents (append brace-ents (list (entlast))))
                  ;; 끝면 연결선 - pA측 캡 (f1-1 ~ f2-1): 맨 처음 세그먼트에만 작도
                  (if (= seg-idx 0)
                    (progn
                      (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 256) '(90 . 2) '(70 . 0)
                                     (list 10 (car f1-1) (cadr f1-1)) (list 10 (car f2-1) (cadr f2-1)))
                                      (list brace-xdata)))
                      (setq brace-ents (append brace-ents (list (entlast))))
                    )
                  )
                  ;; 끝면 연결선 - pB측 캡 (f1-2 ~ f2-2): 맨 마지막 세그먼트에만 작도
                  (if (= seg-idx (1- num-draw-segs))
                    (progn
                      (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 256) '(90 . 2) '(70 . 0)
                                     (list 10 (car f1-2) (cadr f1-2)) (list 10 (car f2-2) (cadr f2-2)))
                                      (list brace-xdata)))
                      (setq brace-ents (append brace-ents (list (entlast))))
                    )
                  )
                  ;; 웹 파선 1 (+half-tw)
                  (setq w1-1 (polar pA (+ seg-ang (/ pi 2.0)) half-tw))
                  (setq w1-2 (polar pB (+ seg-ang (/ pi 2.0)) half-tw))
                  (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 1) '(6 . "DASHED")
                                         (cons 48 (* (if (and (boundp '*tsp-scale-plan*) *tsp-scale-plan*) *tsp-scale-plan* 600.0) (/ 500.0 600.0) (if (= (getvar "MEASUREMENT") 1) (/ 1.0 25.0) 1.0)))
                                         '(90 . 2) '(70 . 0)
                                 (list 10 (car w1-1) (cadr w1-1)) (list 10 (car w1-2) (cadr w1-2)))
                                 (list brace-xdata)))
                  (setq brace-ents (append brace-ents (list (entlast))))
                  ;; 웹 파선 2 (-half-tw)
                  (setq w2-1 (polar pA (- seg-ang (/ pi 2.0)) half-tw))
                  (setq w2-2 (polar pB (- seg-ang (/ pi 2.0)) half-tw))
                  (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 1) '(6 . "DASHED")
                                         (cons 48 (* (if (and (boundp '*tsp-scale-plan*) *tsp-scale-plan*) *tsp-scale-plan* 600.0) (/ 500.0 600.0) (if (= (getvar "MEASUREMENT") 1) (/ 1.0 25.0) 1.0)))
                                         '(90 . 2) '(70 . 0)
                                 (list 10 (car w2-1) (cadr w2-1)) (list 10 (car w2-2) (cadr w2-2)))
                                 (list brace-xdata)))
                  (setq brace-ents (append brace-ents (list (entlast))))
                )
              )
              (setq seg-idx (1+ seg-idx))
            )

            ;; 6. 맞버팀대 중앙 간극 선분 별도 작도 (MID_GAP XDATA로 교차점 검색에서 제외)
            (foreach seg mid-gap-segs
              (setq pA (car seg) pB (cdr seg))
              (if (> (distance pA pB) 0.1)
                (progn
                  (setq seg-ang (angle pA pB))
                  ;; 플랜지 선 1 (+half-b)
                  (setq f1-1 (polar pA (+ seg-ang (/ pi 2.0)) half-b))
                  (setq f1-2 (polar pB (+ seg-ang (/ pi 2.0)) half-b))
                  (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 256) '(90 . 2) '(70 . 0)
                                 (list 10 (car f1-1) (cadr f1-1)) (list 10 (car f1-2) (cadr f1-2)))
                                 (list mid-gap-xdata)))
                  (setq brace-ents (append brace-ents (list (entlast))))
                  ;; 플랜지 선 2 (-half-b)
                  (setq f2-1 (polar pA (- seg-ang (/ pi 2.0)) half-b))
                  (setq f2-2 (polar pB (- seg-ang (/ pi 2.0)) half-b))
                  (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 256) '(90 . 2) '(70 . 0)
                                 (list 10 (car f2-1) (cadr f2-1)) (list 10 (car f2-2) (cadr f2-2)))
                                 (list mid-gap-xdata)))
                  (setq brace-ents (append brace-ents (list (entlast))))
                  ;; 웹 파선 1 (+half-tw)
                  (setq w1-1 (polar pA (+ seg-ang (/ pi 2.0)) half-tw))
                  (setq w1-2 (polar pB (+ seg-ang (/ pi 2.0)) half-tw))
                  (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 1) '(6 . "DASHED")
                                         (cons 48 (* (if (and (boundp '*tsp-scale-plan*) *tsp-scale-plan*) *tsp-scale-plan* 600.0) (/ 500.0 600.0) (if (= (getvar "MEASUREMENT") 1) (/ 1.0 25.0) 1.0)))
                                         '(90 . 2) '(70 . 0)
                                 (list 10 (car w1-1) (cadr w1-1)) (list 10 (car w1-2) (cadr w1-2)))
                                 (list mid-gap-xdata)))
                  (setq brace-ents (append brace-ents (list (entlast))))
                  ;; 웹 파선 2 (-half-tw)
                  (setq w2-1 (polar pA (- seg-ang (/ pi 2.0)) half-tw))
                  (setq w2-2 (polar pB (- seg-ang (/ pi 2.0)) half-tw))
                  (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_보강재(Brace)") '(62 . 1) '(6 . "DASHED")
                                         (cons 48 (* (if (and (boundp '*tsp-scale-plan*) *tsp-scale-plan*) *tsp-scale-plan* 600.0) (/ 500.0 600.0) (if (= (getvar "MEASUREMENT") 1) (/ 1.0 25.0) 1.0)))
                                         '(90 . 2) '(70 . 0)
                                 (list 10 (car w2-1) (cadr w2-1)) (list 10 (car w2-2) (cadr w2-2)))
                                 (list mid-gap-xdata)))
                  (setq brace-ents (append brace-ents (list (entlast))))
                )
              )
            )
          )
        )
      )
    )
  )
  brace-ents
)
