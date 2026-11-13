;;; ==========================================================================
;;; 파일명: TSP_5A_DCL_and_Strut.lsp
;;; 구  분: DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part A)
;;; 역  할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-get-exact-ry, tsp-calc-unsupported-length, tsp-project-dist]
;;; 포함 함수: tsp-get-exact-ry, tsp-calc-unsupported-length, tsp-project-dist
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;; ==========================================================================
;;; [SECTION 6] DCL UI 및 콜백 (Dialog Interface)
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-get-exact-ry : H형강의 제원(H, B, tw, tf)을 적분하여 약축 단면 2차 반경(ry) 정밀 계산
;;;;==========================================================================
(defun tsp-get-exact-ry (h b tw tf / hw Iyw Iyf Iy Area ry)
  (setq hw (- h (* 2.0 tf)))
  (setq Iyw (/ (* hw (expt tw 3.0)) 12.0))
  (setq Iyf (/ (* tf (expt b 3.0)) 6.0))
  (setq Iy (+ Iyw Iyf))
  (setq Area (+ (* 2.0 b tf) (* hw tw)))
  (setq ry (sqrt (/ Iy Area)))
  ry
)

;;;;==========================================================================
;;;; defun tsp-calc-unsupported-length : 세장비 한계와 단면 제원을 바탕으로 허용 비지지길이(m) 산출
;;;;==========================================================================
(defun tsp-calc-unsupported-length (custom-vals slenderness-limit / h b tw tf ry max-len-mm)
  (if (and custom-vals (= (length custom-vals) 4))
    (progn
      (setq h (float (nth 0 custom-vals)))
      (setq b (float (nth 1 custom-vals)))
      (setq tw (float (nth 2 custom-vals)))
      (setq tf (float (nth 3 custom-vals)))
      (setq ry (tsp-get-exact-ry h b tw tf))
    )
    (setq ry (* 0.25 300.0)) ; 파싱 실패 시 기본값 H-300 약산 적용
  )
  (setq max-len-mm (* slenderness-limit ry))
  (/ max-len-mm 1000.0) ; m 단위 반환
)

;;;;==========================================================================
;;;; defun tsp-project-dist : 특정 각도(dir) 축에 대한 목표점의 투영 거리 산출 (스칼라 내적)
;;;;==========================================================================
(defun tsp-project-dist (base-pt dir-angle target-pt / vec target-vec dot)
  (setq vec (list (cos dir-angle) (sin dir-angle)))
  (setq target-vec (list (- (car target-pt) (car base-pt)) (- (cadr target-pt) (cadr base-pt))))
  (setq dot (+ (* (car vec) (car target-vec)) (* (cadr vec) (cadr target-vec))))
  dot
)
