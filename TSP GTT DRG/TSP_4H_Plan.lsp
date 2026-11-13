;;; ==========================================================================
;;; 파일명: TSP_4H_Plan.lsp
;;; 구  분: 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part H)
;;; 역  할: 평면 띠장 오프셋/상향, H-Pile/토류판 배치, 사보강재/화타쐐기, 차수그라우팅, 연속 CIP 평면 배열, 세그먼트/모서리 번호표 및 수동말뚝 삭제 [tsp-ang-dist, get-grouting-arc-angles, tsp-math-normalize-angle, tsp-draw-advanced-grouting]
;;; 포함 함수: tsp-ang-dist, get-grouting-arc-angles, tsp-math-normalize-angle, tsp-draw-advanced-grouting
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-ang-dist : 두 각도의 차이(최단거리) 산출
;;;;==========================================================================
(defun tsp-ang-dist (a b / d)
  (setq d (abs (- a b)))
  (if (> d pi) (- (* 2.0 pi) d) d)
)

;;;;==========================================================================
;;;; defun get-grouting-arc-angles : 그라우팅 원과 CIP 원의 겹침을 잘라내기 위한 시작/끝 각도 산출
;;;;==========================================================================
(defun get-grouting-arc-angles (G C1 C2 R_G R_C out-norm / d1 d2 theta1 theta2 a1 a2 a1A a1B a2A a2B E S diff1A diff1B diff2A diff2B S-out S-E)
  (setq d1 (distance G C1) d2 (distance G C2))
  (setq S nil E nil)
  (if (and (< d2 (+ R_G R_C)) (> d2 (abs (- R_G R_C))))
    (progn
      (setq theta2 (tsp-acos (/ (+ (* R_G R_G) (* d2 d2) (- (* R_C R_C))) (* 2.0 R_G d2))))
      (setq a2 (angle G C2))
      (setq a2A (tsp-normalize-angle (+ a2 theta2)))
      (setq a2B (tsp-normalize-angle (- a2 theta2)))
      (setq diff2A (tsp-ang-dist a2A out-norm))
      (setq diff2B (tsp-ang-dist a2B out-norm))
      (if (< diff2A diff2B) (setq S a2A) (setq S a2B))
    )
  )
  (if (and (< d1 (+ R_G R_C)) (> d1 (abs (- R_G R_C))))
    (progn
      (setq theta1 (tsp-acos (/ (+ (* R_G R_G) (* d1 d1) (- (* R_C R_C))) (* 2.0 R_G d1))))
      (setq a1 (angle G C1))
      (setq a1A (tsp-normalize-angle (+ a1 theta1)))
      (setq a1B (tsp-normalize-angle (- a1 theta1)))
      (setq diff1A (tsp-ang-dist a1A out-norm))
      (setq diff1B (tsp-ang-dist a1B out-norm))
      (if (< diff1A diff1B) (setq E a1A) (setq E a1B))
    )
  )
  (if (and S E)
    (progn
      (setq S-out (tsp-normalize-angle (- out-norm S)))
      (setq S-E (tsp-normalize-angle (- E S)))
      (if (<= S-out S-E)
        (list S E)
        (list E S)
      )
    )
    nil
  )
)

;;;;==========================================================================
;;;; defun tsp-math-normalize-angle : 각도를 0 ~ 2PI 사이로 정규화
;;;;==========================================================================
(defun tsp-math-normalize-angle (ang)
  (while (< ang 0.0) (setq ang (+ ang (* 2.0 pi))))
  (while (>= ang (* 2.0 pi)) (setq ang (- ang (* 2.0 pi))))
  ang
)

;;;;==========================================================================
;;;; defun tsp-draw-advanced-grouting : 수학적 연산을 통한 완벽한 교차 호(Arc) 작도 엔진
;;;;==========================================================================
(defun tsp-draw-advanced-grouting (G-pt grouting-dia cip-centers cip-dia / R_G R_C blocks completely-eaten dist-C cos-val ang-offset ang-C S E merged cur gap-S gap-E diff)
  (setq R_G (/ grouting-dia 2.0))
  (setq R_C (/ cip-dia 2.0))

  (setq blocks '())
  (setq completely-eaten nil)

  ;; 1. 모든 CIP와의 교차 여부 검사하여 제거될 각도 구간(Bite) 수집
  (foreach C-pt cip-centers
    (setq dist-C (distance G-pt C-pt))
    (if (<= dist-C (- R_C R_G -0.01))
      (setq completely-eaten T) ; CIP 안으로 그라우팅이 완전히 파묻힘 (트림 실패 버그 해결)
      (if (< dist-C (+ R_G R_C -0.01))
        (progn
          (setq cos-val (/ (- (+ (* R_G R_G) (* dist-C dist-C)) (* R_C R_C)) (* 2.0 R_G dist-C)))
          (setq cos-val (max -1.0 (min 1.0 cos-val)))
          (setq ang-offset (atan (sqrt (- 1.0 (* cos-val cos-val))) cos-val))
          (setq ang-C (angle G-pt C-pt))

          (setq S (tsp-math-normalize-angle (- ang-C ang-offset)))
          (setq E (tsp-math-normalize-angle (+ ang-C ang-offset)))

          ;; 0도를 걸치는 경우 구간을 2개로 분할
          (if (> S E)
            (setq blocks (append blocks (list (list S (* 2.0 pi)) (list 0.0 E))))
            (setq blocks (append blocks (list (list S E))))
          )
        )
      )
    )
  )

  (if (not completely-eaten)
    (progn
      ;; 2. 제거될 구간(블록) 병합 (Union) - 호 쪼개짐 현상 해결
      (if blocks
        (progn
          (setq blocks (vl-sort blocks (function (lambda (a b) (< (car a) (car b))))))
          (setq merged '())
          (setq cur (car blocks))
          (foreach b (cdr blocks)
            (if (<= (car b) (+ (cadr cur) 1e-6))
              (setq cur (list (car cur) (max (cadr cur) (cadr b))))
              (progn
                (setq merged (append merged (list cur)))
                (setq cur b)
              )
            )
          )
          (if cur (setq merged (append merged (list cur))))

          ;; 3. 병합된 블록 사이의 빈 공간(Gap)을 하나의 완벽한 호(Arc)로 작도
          (setq i 0)
          (while (< i (1- (length merged)))
            (setq gap-S (cadr (nth i merged)))
            (setq gap-E (car (nth (1+ i) merged)))
            (setq diff (- gap-E gap-S))
            (if (> diff 1e-4)
              (entmake (list '(0 . "ARC") (cons 8 "_그라우팅") (cons 62 5) (cons 10 G-pt) (cons 40 R_G) (cons 50 gap-S) (cons 51 gap-E)))
            )
            (setq i (1+ i))
          )

          ;; 마지막 블록과 첫 번째 블록 사이의 간격 (0도를 통과하는 단일 호 처리)
          (setq gap-S (cadr (last merged)))
          (setq gap-E (car (nth 0 merged)))
          (setq diff (if (>= gap-E gap-S) (- gap-E gap-S) (+ gap-E (- (* 2.0 pi) gap-S))))
          (if (> diff 1e-4)
            (entmake (list '(0 . "ARC") (cons 8 "_그라우팅") (cons 62 5) (cons 10 G-pt) (cons 40 R_G) (cons 50 gap-S) (cons 51 gap-E)))
          )
        )
        ;; 겹치는 CIP가 없으면 온전한 원 생성
        (entmake (list '(0 . "CIRCLE") (cons 8 "_그라우팅") (cons 62 5) (cons 10 G-pt) (cons 40 R_G)))
      )

      ;; D600 특수 코어 마커
      (if (= grouting-dia 600.0)
        (entmake (list '(0 . "CIRCLE") (cons 8 "_그라우팅") (cons 62 5) (cons 10 G-pt) (cons 40 25.0)))
      )
    )
  )
)
