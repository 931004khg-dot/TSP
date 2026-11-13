;;; ==========================================================================
;;; 파일명: TSP_4B_Plan.lsp
;;; 구  분: 평면도 벽체(H-Pile/CIP/띠장/그라우팅), 사보강재 및 번호표 작도 (Part B)
;;; 역  할: 평면 띠장 오프셋/상향, H-Pile/토류판 배치, 사보강재/화타쐐기, 차수그라우팅, 연속 CIP 평면 배열, 세그먼트/모서리 번호표 및 수동말뚝 삭제 [calculate-corner-params, get-hpile-limit-offset, create-hpile-section, create-hpile-section-block, create-post-pile-block 외 5개]
;;; 포함 함수: calculate-corner-params, get-hpile-limit-offset, create-hpile-section, create-hpile-section-block, create-post-pile-block, create-timber-panel-object, place-hpile-at-corner-simple, tsp-geo-station, tsp-geo-augment-chain, tsp-geo-arc-of
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun calculate-corner-params : 코너 부위에서 H-Pile의 회전각 및 삽입 위치 계산
;;;;==========================================================================
(defun calculate-corner-params (vertex prev-vertex next-vertex h b timber-offset boundary-orient / is-convex half-b angle1 angle2 prev-dir next-dir wale-corner wale-corner-angle hpile-rotation offset-dir insert-point half-angle offset-dist)
  (setq half-b (/ b 2.0))
  ;; 1. 코너 방향 판별 (볼록/오목)
  (setq is-convex (is-corner-convex prev-vertex vertex next-vertex boundary-orient))
  ;; 2. 각도 계산
  (setq angle1 (angle prev-vertex vertex))
  (setq angle2 (angle vertex next-vertex))
  ;; 방향 벡터 설정
  (setq prev-dir (+ angle1 (* boundary-orient (/ pi 2.0))))
  (setq next-dir (+ angle2 (* boundary-orient (/ pi 2.0))))
  ;; 3. 이등분선(Wale Corner) 위치 및 각도 계산
  (setq wale-corner (list
    (/ (+ (car (polar vertex prev-dir timber-offset)) (car (polar vertex next-dir timber-offset))) 2.0)
    (/ (+ (cadr (polar vertex prev-dir timber-offset)) (cadr (polar vertex next-dir timber-offset))) 2.0)
    0.0))
  (setq wale-corner-angle (angle vertex wale-corner))
  ;; 4. H-Pile 회전 각도 결정 (이등분선 + 90도)
  (setq hpile-rotation (+ wale-corner-angle (/ pi 2.0)))
  ;; 5. 삽입점(Insert Point) 결정
  (if is-convex
    (progn
      ;; [볼록한 경우] - 정점에 배치
      (setq insert-point vertex)
    )
    (progn
      ;; [오목한 경우] - 사용자 지정 공식 적용
      ;; 공식: l = B/2 * tan(90-a)
      ;; 여기서 a는 half-angle
      ;; a (Half Angle) 계산
      (setq half-angle (abs (- angle2 wale-corner-angle)))
      ;; 각도 정규화 (PI보다 크면 반대쪽 각도)
      (if (> half-angle pi) (setq half-angle (- (* 2.0 pi) half-angle)))
      ;; 0으로 나누기 방지 (매우 뾰족한 경우 안전장치)
      (if (< half-angle 0.001) (setq half-angle 0.001))
      ;; 거리 l 계산: tan(90-a) = cot(a) = cos(a)/sin(a)
      (setq offset-dist (* half-b (/ (cos half-angle) (sin half-angle))))
      ;; 방향: 이등분선 방향으로 이격
      (setq offset-dir wale-corner-angle)
      ;; 최종 좌표 계산
      (setq insert-point (polar vertex offset-dir offset-dist))
    )
  )
  ;; 결과 반환
  (list insert-point hpile-rotation is-convex)
)

;;;;==========================================================================
;;;; defun get-hpile-limit-offset : H-Pile 배치 시 플랜지 간섭을 피하기 위한 한계 오프셋 계산
;;;;==========================================================================
(defun get-hpile-limit-offset (hpile-pt h b tf rotation seg-origin seg-angle is-start is-convex / half-h half-b corners pt dx dy proj-dist best-dist wx wy)
  (setq half-h (/ h 2.0) half-b (/ b 2.0))
  (if is-convex
    (setq corners (list
      (list (- half-b) 0)
      (list half-b 0)
      (list half-b h)
      (list (- half-b) h)
    ))
    (setq corners (list
      (list (- half-b) tf)
      (list half-b tf)
    ))
  )
  (setq best-dist (if is-start -1e99 1e99))
  (foreach pt corners
    (setq wx (+ (car hpile-pt) (- (* (car pt) (cos rotation)) (* (cadr pt) (sin rotation)))))
    (setq wy (+ (cadr hpile-pt) (+ (* (car pt) (sin rotation)) (* (cadr pt) (cos rotation)))))
    (setq dx (- wx (car seg-origin)))
    (setq dy (- wy (cadr seg-origin)))
    (setq proj-dist (+ (* dx (cos seg-angle)) (* dy (sin seg-angle))))
    (if is-start
      (if (> proj-dist best-dist) (setq best-dist proj-dist))
      (if (< proj-dist best-dist) (setq best-dist proj-dist))
    )
  )
  best-dist
)

;;;;==========================================================================
;;;; defun create-hpile-section : H-Pile 폴리라인 형상 생성
;;;;==========================================================================
(defun create-hpile-section (insert-pt h b tw tf layer-name / pts)
  ;; 공통 함수를 통해 좌표 리스트 획득 (insert-pt가 이미 중심점)
  (setq pts (calc-hpile-vertices insert-pt h b tw tf))
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
    (cons 8 layer-name) (cons 62 3) '(90 . 20) '(70 . 1)
    (cons 10 (nth 0 pts))  (cons 10 (nth 1 pts))
    (cons 10 (nth 2 pts))  (cons 42 0.4142135623730951) (cons 10 (nth 3 pts))
    (cons 10 (nth 4 pts))  (cons 42 0.4142135623730951) (cons 10 (nth 5 pts))
    (cons 10 (nth 6 pts))  (cons 10 (nth 7 pts))
    (cons 10 (nth 8 pts))  (cons 10 (nth 9 pts))
    (cons 10 (nth 10 pts)) (cons 42 0.4142135623730951) (cons 10 (nth 11 pts))
    (cons 10 (nth 12 pts)) (cons 42 0.4142135623730951) (cons 10 (nth 13 pts))
    (cons 10 (nth 14 pts)) (cons 10 (nth 15 pts))))
  (entlast)
)

;;;;==========================================================================
;;;; defun create-hpile-section-block : H-Pile 단면 형상을 블록으로 정의
;;;;==========================================================================
(defun create-hpile-section-block (h b tw tf / hpile-ent point-ent block-name layer-name old-ucs old-osnap half-h)
  (setq block-name (strcat "_HPILE(" (itoa h) "x" (itoa b) "x" (itoa tw) "x" (itoa tf) ")"))
  (setq layer-name (strcat "_H-Pile_" (itoa h) "x" (itoa b) "x" (itoa tw) "-" (itoa tf)))
  (create-layer-if-not-exists layer-name "3")
  (create-layer-if-not-exists "_측면말뚝" "3")
  (if (not (tblsearch "BLOCK" block-name))
    (progn
      (setq old-ucs (getvar "UCSNAME") old-osnap (getvar "OSMODE"))
      (command "._UCS" "_W") (setvar "OSMODE" 0)
      (setq hpile-ent (create-hpile-section '(0 0) h b tw tf layer-name))
      (setq half-h (/ h 2.0))
      (entmake (list '(0 . "POINT") '(8 . "_측면말뚝") (cons 10 (list 0.0 (- half-h) 0.0))))
      (setq point-ent (entlast))
      (command "._-BLOCK" block-name (list 0.0 (- half-h) 0.0) hpile-ent point-ent "")
      (if old-ucs (command "._UCS" "_R" old-ucs) (command "._UCS" "_W"))
      (setvar "OSMODE" old-osnap)
    )
  )
  block-name
)

;;;;==========================================================================
;;;; defun create-post-pile-block : 중간말뚝(Post Pile) 블록 정의
;;;;==========================================================================
(defun create-post-pile-block (h b tw tf / hpile-ent point-ent point-ent2 block-name layer-name old-ucs old-osnap half-h half-b ss)
  (setq block-name (strcat "_POST_PILE(" (itoa (fix h)) "x" (itoa (fix b)) "x" (itoa (fix tw)) "x" (itoa (fix tf)) ")"))
  (setq layer-name (strcat "_H-Pile_" (itoa (fix h)) "x" (itoa (fix b)) "x" (itoa (fix tw)) "-" (itoa (fix tf))))
  (create-layer-if-not-exists layer-name "3")
  (create-layer-if-not-exists "_중간말뚝(Post pile)" "3")
  (if (not (tblsearch "BLOCK" block-name))
    (progn
      (setq old-ucs (getvar "UCSNAME") old-osnap (getvar "OSMODE"))
      (command "._UCS" "_W") (setvar "OSMODE" 0)
      (setq hpile-ent (create-hpile-section '(0 0) h b tw tf layer-name))
      (setq half-h (/ h 2.0))
      (setq half-b (/ b 2.0))

      (entmake (list '(0 . "POINT") '(8 . "_중간말뚝(Post pile)") (cons 10 (list 0.0 (- half-h) 0.0))))
      (setq point-ent (entlast))

      ;; 플랜지 아랫면 왼쪽 모서리에 추가 POINT 생성 (- half-b 적용)
      (entmake (list '(0 . "POINT") '(8 . "_중간말뚝(Post pile)") (cons 10 (list (- half-b) (- half-h) 0.0))))
      (setq point-ent2 (entlast))

      ;; 에러 방지를 위해 객체들을 하나의 선택 세트(Selection Set)로 묶음
      (setq ss (ssadd))
      (ssadd hpile-ent ss)
      (ssadd point-ent ss)
      (ssadd point-ent2 ss)

      ;; 묶인 선택 세트(ss)를 이용해 블록 생성
      (command "._-BLOCK" block-name (list 0.0 (- half-h) 0.0) ss "")
      (if old-ucs (command "._UCS" "_R" old-ucs) (command "._UCS" "_W"))
      (setvar "OSMODE" old-osnap)
    )
  )
  block-name
)

;;;;==========================================================================
;;;; defun create-timber-panel-object : 평면도 토류판 폴리라인 및 해치 생성
;;;;==========================================================================
(defun create-timber-panel-object (center-pt width height rot-angle / half-width half-height cos-a sin-a pt1 pt2 pt3 pt4 timber-pline hatch-obj sa doc mspace top-left top-right origin-sa)
  (setq half-width (/ width 2.0) half-height (/ height 2.0))
  (setq cos-a (cos rot-angle) sin-a (sin rot-angle))
  (setq pt1 (list (+ (car center-pt) (- (* (- half-width) cos-a) (* (- half-height) sin-a))) (+ (cadr center-pt) (+ (* (- half-width) sin-a) (* (- half-height) cos-a)))))
  (setq pt2 (list (+ (car center-pt) (- (* half-width cos-a) (* (- half-height) sin-a))) (+ (cadr center-pt) (+ (* half-width sin-a) (* (- half-height) cos-a)))))
  (setq pt3 (list (+ (car center-pt) (- (* half-width cos-a) (* half-height sin-a))) (+ (cadr center-pt) (+ (* half-width sin-a) (* half-height cos-a)))))
  (setq pt4 (list (+ (car center-pt) (- (* (- half-width) cos-a) (* half-height sin-a))) (+ (cadr center-pt) (+ (* (- half-width) sin-a) (* half-height cos-a)))))
  (setq top-right pt3)
  (setq top-left pt4)
  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_토류판(timber)") '(62 . 1) '(90 . 4) '(70 . 1) (cons 10 pt1) (cons 10 pt2) (cons 10 pt3) (cons 10 pt4)))
  (setq timber-pline (entlast))
  (if timber-pline
    (progn
      (setq doc (vla-get-activedocument (vlax-get-acad-object)))
      (setq mspace (vla-get-modelspace doc))
      (setq hatch-obj (vla-addhatch mspace 1 "ANSI36" :vlax-true))
      (vla-put-patternscale hatch-obj (* 80.0 (get-hatch-scale-factor)))
      (vla-put-patternangle hatch-obj (* 15.0 (/ pi 180.0)))
      (vla-put-layer hatch-obj "_토류판(timber)")
      (vla-put-color hatch-obj 9)
      (setq sa (vlax-make-safearray 9 (cons 0 0)))
      (vlax-safearray-fill sa (list (vlax-ename->vla-object timber-pline)))
      (vla-appendouterloop hatch-obj sa)
      (setq origin-sa (vlax-make-safearray 5 '(0 . 1)))
      (vlax-safearray-fill origin-sa (list (car center-pt) (cadr center-pt)))
      (vla-put-origin hatch-obj origin-sa)
      (vla-evaluate hatch-obj)
    )
  )
  (entmake (list '(0 . "POINT") '(8 . "_토류판(timber)") '(62 . 1) (cons 10 top-left)))
  (entmake (list '(0 . "POINT") '(8 . "_토류판(timber)") '(62 . 1) (cons 10 top-right)))
  (list timber-pline top-left top-right)
)

;;;;==========================================================================
;;;; defun place-hpile-at-corner-simple : 코너 부위에 단일 H-Pile 블록 배치
;;;;==========================================================================
(defun place-hpile-at-corner-simple (vertex angle1 angle2 h b tw tf hpile-block timber-offset boundary-orient prev-vertex next-vertex / hpile-info insert-point hpile-rotation is-convex half-b local-y flange-left flange-right)
  (setq half-b (/ b 2.0))
  (setq hpile-info (calculate-corner-params vertex prev-vertex next-vertex h b timber-offset boundary-orient))
  (setq insert-point (car hpile-info))
  (setq hpile-rotation (cadr hpile-info))
  (setq is-convex (nth 2 hpile-info))
  (entmake (list '(0 . "INSERT") (cons 2 hpile-block) '(8 . "_측면말뚝") (cons 10 insert-point) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 hpile-rotation)))
  ;; 생성된 H-Pile 삽입점(플랜지 바닥 중심) 캐싱
  (setq *tsp-hpile-points* (append *tsp-hpile-points* (list insert-point)))
  (setq local-y (if is-convex 0.0 tf))
  (setq flange-left (list
    (+ (car insert-point) (- (* (- half-b) (cos hpile-rotation)) (* local-y (sin hpile-rotation))))
    (+ (cadr insert-point) (+ (* (- half-b) (sin hpile-rotation)) (* local-y (cos hpile-rotation))))
    0.0))
  (setq flange-right (list
    (+ (car insert-point) (- (* half-b (cos hpile-rotation)) (* local-y (sin hpile-rotation))))
    (+ (cadr insert-point) (+ (* half-b (sin hpile-rotation)) (* local-y (cos hpile-rotation))))
    0.0))
  (create-layer-if-not-exists "DEBUG_FLANGE" "2")
  (entmake (list '(0 . "POINT") '(8 . "DEBUG_FLANGE") '(62 . 2) (cons 10 flange-left)))
  (entmake (list '(0 . "POINT") '(8 . "DEBUG_FLANGE") '(62 . 2) (cons 10 flange-right)))
)

;;;;==========================================================================
;;;; defun tsp-geo-station : [경계선 추종] 설계 현(chord) 기준 거리 v 를
;;;;   경계선 실제 호장(arc-length) 위치로 변환한다.
;;;;   ※ 선형 변환이므로 말뚝 간격(C.T.C)이 호장 기준으로 정확히 보존된다.
;;;;   [보강] plen < 0 (개방형 경계선 역방향 진행) 지원 및 접선각 180도(pi) 반전 보정
;;;;   반환: (점 접선각)  또는 nil
;;;;==========================================================================
(defun tsp-geo-station (v d1 len plen total closed vobj / d p prm prm-safe end-prm der tang)
  (setq p nil tang nil)
  (setq d (tsp-geo-arc-of v d1 len plen total closed))
  (if (and vobj d (>= d 0.0))
    (progn
      (setq p (vl-catch-all-apply 'vlax-curve-getpointatdist (list vobj d)))
      (if (vl-catch-all-error-p p) (setq p nil))
      (if p
        (progn
          (setq prm (vl-catch-all-apply 'vlax-curve-getparamatdist (list vobj d)))
          (if (not (vl-catch-all-error-p prm))
            (progn
              (setq prm-safe prm)
              (setq end-prm (vl-catch-all-apply 'vlax-curve-getendparam (list vobj)))
              (if (and (not (vl-catch-all-error-p end-prm)) (equal prm-safe (fix prm-safe) 1e-4))
                (if (< plen 0.0)
                  (if (> prm-safe 0.0) (setq prm-safe (- prm-safe 1e-4)) (setq prm-safe (+ prm-safe 1e-4)))
                  (if (< prm-safe end-prm) (setq prm-safe (+ prm-safe 1e-4)) (setq prm-safe (- prm-safe 1e-4)))
                )
              )
              (setq der (vl-catch-all-apply 'vlax-curve-getfirstderiv (list vobj prm-safe)))
              (if (not (vl-catch-all-error-p der))
                (progn
                  (setq tang (angle (list 0.0 0.0 0.0) der))
                  ;; 개방형 역방향(plen < 0)일 때는 접선 벡터가 반대이므로 +pi 보정
                  (if (< plen 0.0)
                    (setq tang (rem (+ tang pi) (* 2.0 pi)))
                  )
                )
              )
            )
          )
          (setq p (list (car p) (cadr p) 0.0))
        )
      )
    )
  )
  (if p (list p tang) nil)
)

;;;;==========================================================================
;;;; defun tsp-geo-augment-chain : [경계선 추종] 설계 체인점 사이에 경계선의 실제 정점을 삽입
;;;;   (모서리 삭제로 설계 목록에서 빠진 정점이 CIP/그라우팅 오프셋 경로에서도 유지되도록 함)
;;;;==========================================================================
(defun tsp-geo-augment-chain (boundary-ent chain-pts / info raw dlist total is-closed vobj out n i a b da db wrap wd rev-chain added k ok-a ok-b pr)
  (setq info (tsp-boundary-path-info boundary-ent))
  (if (or (null info) (null chain-pts) (< (length chain-pts) 2))
    chain-pts
    (progn
      (setq raw (nth 0 info) dlist (nth 1 info) total (nth 2 info) is-closed (nth 3 info))
      (setq vobj (vl-catch-all-apply 'vlax-ename->vla-object (list boundary-ent)))
      (if (vl-catch-all-error-p vobj) (setq vobj nil))
      (setq out '() n (length chain-pts) i 0)
      (while (< i n)
        (setq a (nth i chain-pts))
        (setq out (append out (list a)))
        (setq b (if (< (1+ i) n) (nth (1+ i) chain-pts) (if is-closed (car chain-pts) nil)))
        (if (and b vobj (> (distance a b) 1.0))
          (progn
            (setq ok-a (< (distance a (vlax-curve-getclosestpointto vobj a)) 500.0))
            (setq ok-b (< (distance b (vlax-curve-getclosestpointto vobj b)) 500.0))
            (if (and ok-a ok-b)
              (progn
                (setq da (tsp-dist-of-near-vertex raw dlist a) db (tsp-dist-of-near-vertex raw dlist b))
                (if (and da db)
                  (progn
                    ;; [개방형 역방향 지원] db < da 인 구간은 후방(b<-a) 탐색으로 올바른 중간 정점만 삽입
                    ;; (기존: 전방 wrap 공식 때문에 경계선 반대편 정점이 삽입되어 자기교차 경로 생성)
                    (setq rev-chain (and (not is-closed) (< db da)))
                    (setq wrap (if rev-chain (- da db) (if (>= db da) (- db da) (+ (- total da) db))))
                    (setq added '() k 0)
                    (foreach pr raw
                      (setq wd (cond
                                 (rev-chain (if (<= (nth k dlist) da) (- da (nth k dlist)) nil))
                                 ((>= (nth k dlist) da) (- (nth k dlist) da))
                                 (t (+ (- total da) (nth k dlist)))
                               )
                      )
                      (if (and wd (> wd 0.5) (< wd (- wrap 0.5)))
                        (setq added (append added (list (list wd pr))))
                      )
                      (setq k (1+ k))
                    )
                    (if added
                      (foreach pr (vl-sort added '(lambda (x y) (< (car x) (car y))))
                        (setq out (append out (list (cadr pr))))
                      )
                    )
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
      out
    )
  )
)

;;;;==========================================================================
;;;; defun tsp-geo-arc-of : [경계선 추종] 현 기준 거리 v -> 경계선 호장 위치(절대값)
;;;;   [보강] plen < 0 (개방형 역방향 구간: d2 < d1)일 때도 호장 위치를 정확히 계산
;;;;==========================================================================
(defun tsp-geo-arc-of (v d1 len plen total closed / d abs-plen)
  (setq d nil)
  (if (and d1 plen len (> len 0.0001) (> (abs plen) 0.0001))
    (progn
      (setq abs-plen (abs plen))
      (if (< plen 0.0)
        (setq d (- d1 (+ (/ abs-plen 2.0) (- v (/ len 2.0)))))
        (setq d (+ d1 (/ abs-plen 2.0) (- v (/ len 2.0))))
      )
      (if (and closed total (> total 0.0001))
        (progn
          (while (< d 0.0) (setq d (+ d total)))
          (while (>= d total) (setq d (- d total)))
        )
        (if (and total (> total 0.0001))
          (setq d (max 0.0 (min total d)))
        )
      )
    )
  )
  d
)
