;;; ==========================================================================
;;; 파일명: TSP_5M_DCL_and_Strut.lsp
;;; 구  분: DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part M)
;;; 역  할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-shift-anchor-angles, tsp-get-support-item-wale-spec, tsp-get-support-item-wale-vals, tsp-get-support-item-range-pts, tsp-set-support-item-range-pts 외 29개]
;;; 포함 함수: tsp-shift-anchor-angles, tsp-get-support-item-wale-spec, tsp-get-support-item-wale-vals, tsp-get-support-item-range-pts, tsp-set-support-item-range-pts, tsp-get-support-item-dist-interval, tsp-shift-support-ranges, tsp-shift-anchor-angles-in-segs, tsp-build-poly-verts, tsp-is-point-inside, tsp-brace-opt-get, tsp-brace-opt-set, tsp-corbel-deleted-p, tsp-corbel-adopt-ents, tsp-shift-deleted-corbels, tsp-manual-delete-corbel, tsp-reset-deleted-corbels, tsp-corbel-default-str, tsp-ctc-mm, tsp-1h-post-quadrant, tsp-1h-post-place, tsp-1h-post-reposition, tsp-2h-post-reposition, tsp-1h-jack-want-top, tsp-1h-jack-at-pt2, tsp-norm-angle-rad, tsp-angle-bisector, tsp-calc-strut-step-dist, tsp-get-seg-inward-normal, tsp-poly-from-segs, tsp-poly-center, tsp-pt-line-dist, tsp-wale-lines-geo, tsp-wale-lines-dwg
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-shift-anchor-angles : 지보재 목록 내 앵커 평면각 기록(9번 인덱스) 좌표 이동 보정
;;;;   - 기록 형식 : ((기준점 . 각도) ...)  기준점은 절대좌표이므로 경계선 이동 시 함께 이동 필요
;;;;==========================================================================
(defun tsp-shift-anchor-angles (sup-list dx dy / out item ang-list new-item idx)
  (setq out '())
  (foreach item sup-list
    (if (and (listp item) item (= (type (car item)) 'STR) (wcmatch (car item) "*앵커*")
             (> (length item) 9) (nth 9 item) (listp (nth 9 item)))
      (progn
        (setq ang-list '())
        (foreach rec (nth 9 item)
          (if (and (listp rec) (listp (car rec)) (numberp (car (car rec))) (numberp (cadr (car rec))))
            (setq ang-list (append ang-list (list (cons (list (+ (car (car rec)) dx) (+ (cadr (car rec)) dy) 0.0) (cdr rec)))))
            (setq ang-list (append ang-list (list rec)))
          )
        )
        (setq idx 0 new-item '())
        (foreach v item
          (setq new-item (append new-item (list (if (= idx 9) ang-list v))))
          (setq idx (1+ idx))
        )
        (setq out (append out (list new-item)))
      )
      (setq out (append out (list item)))
    )
  )
  out
)

;;;;==========================================================================
;;;; defun tsp-get-support-item-wale-spec : 개별 지보재 항목의 띠장 규격 문자열 반환 (미지정 시 기본 띠장 반환)
;;;;==========================================================================
(defun tsp-get-support-item-wale-spec (s-item default-spec / s-type w-spec)
  (setq w-spec nil)
  (if (and (listp s-item) (> (length s-item) 0))
    (progn
      (setq s-type (car s-item))
      (cond
        ((wcmatch s-type "*앵커*")
         (if (and (> (length s-item) 10) (nth 10 s-item) (= (type (nth 10 s-item)) 'STR) (/= (nth 10 s-item) ""))
           (setq w-spec (nth 10 s-item))
         )
        )
        ((wcmatch s-type "버팀보*")
         (if (and (> (length s-item) 12) (nth 12 s-item) (= (type (nth 12 s-item)) 'STR) (/= (nth 12 s-item) ""))
           (setq w-spec (nth 12 s-item))
         )
        )
      )
    )
  )
  (if (and w-spec (/= w-spec "")) w-spec (if default-spec default-spec *tsp-wale-spec*))
)

;;;;==========================================================================
;;;; defun tsp-get-support-item-wale-vals : 개별 지보재 항목의 띠장 제원 리스트 (H B tw tf) 반환
;;;;==========================================================================
(defun tsp-get-support-item-wale-vals (s-item default-spec default-custom / spec vals)
  (setq spec (tsp-get-support-item-wale-spec s-item default-spec))
  (if (and (= spec "User-defined") default-custom)
    (setq vals default-custom)
    (setq vals (parse-h-spec spec))
  )
  (if (null vals)
    (setq vals (if default-custom default-custom '(300 300 10 15)))
  )
  vals
)

;;;;==========================================================================
;;;; defun tsp-get-support-item-range-pts : 개별 지보재 항목에 지정된 적용 구간 좌표 (pt1 pt2) 반환 (전체면 nil)
;;;;==========================================================================
(defun tsp-get-support-item-range-pts (s-item / s-type r-pts)
  (setq r-pts nil)
  (if (and (listp s-item) (> (length s-item) 0))
    (progn
      (setq s-type (car s-item))
      (cond
        ((wcmatch s-type "*앵커*")
         (if (and (> (length s-item) 11) (nth 11 s-item) (listp (nth 11 s-item)) (= (length (nth 11 s-item)) 2))
           (setq r-pts (nth 11 s-item))
         )
        )
        ((wcmatch s-type "버팀보*")
         (if (and (> (length s-item) 13) (nth 13 s-item) (listp (nth 13 s-item)) (= (length (nth 13 s-item)) 2))
           (setq r-pts (nth 13 s-item))
         )
        )
      )
    )
  )
  r-pts
)

;;;;==========================================================================
;;;; defun tsp-set-support-item-range-pts : 개별 지보재 항목에 적용 구간 좌표 (pt1 pt2) 또는 nil 저장
;;;;==========================================================================
(defun tsp-set-support-item-range-pts (s-item range-pts default-wale-spec / s-type w-spec)
  (setq s-type (car s-item))
  (setq w-spec (tsp-get-support-item-wale-spec s-item default-wale-spec))
  (cond
    ((wcmatch s-type "*앵커*")
     (while (< (length s-item) 10) (setq s-item (append s-item (list nil))))
     (if (= (length s-item) 10) (setq s-item (append s-item (list w-spec))))
     (if (= (length s-item) 11) (setq s-item (append s-item (list range-pts))))
     (setq s-item (update-list-item s-item 10 w-spec))
     (setq s-item (update-list-item s-item 11 range-pts))
    )
    ((wcmatch s-type "버팀보*")
     (while (< (length s-item) 12) (setq s-item (append s-item (list nil))))
     (if (= (length s-item) 12) (setq s-item (append s-item (list w-spec))))
     (if (= (length s-item) 13) (setq s-item (append s-item (list range-pts))))
     (setq s-item (update-list-item s-item 12 w-spec))
     (setq s-item (update-list-item s-item 13 range-pts))
    )
  )
  s-item
)

;;;;==========================================================================
;;;; defun tsp-get-support-item-dist-interval : 세그먼트 시작점(v-start)과 각도(v-angle) 기준으로 적용 구간 (d1 . d2) 반환
;;;;==========================================================================
(defun tsp-get-support-item-dist-interval (s-item v-start v-angle seg-len / r-pts p1 p2 d1 d2)
  (setq r-pts (tsp-get-support-item-range-pts s-item))
  (if (and r-pts (= (length r-pts) 2) (listp (car r-pts)) (listp (cadr r-pts)))
    (progn
      (setq p1 (car r-pts) p2 (cadr r-pts))
      (setq d1 (+ (* (- (car p1) (car v-start)) (cos v-angle)) (* (- (cadr p1) (cadr v-start)) (sin v-angle))))
      (setq d2 (+ (* (- (car p2) (car v-start)) (cos v-angle)) (* (- (cadr p2) (cadr v-start)) (sin v-angle))))
      (setq d1 (max 0.0 (min seg-len d1)))
      (setq d2 (max 0.0 (min seg-len d2)))
      (cons (min d1 d2) (max d1 d2))
    )
    (cons 0.0 seg-len)
  )
)

;;;;==========================================================================
;;;; defun tsp-shift-support-ranges : 경계선 이동 시 지보재 적용 구간 좌표(range-pts)도 함께 이동 보정
;;;;==========================================================================
(defun tsp-shift-support-ranges (sup-list dx dy / out item r-pts new-r-pts s-type idx new-item v)
  (setq out '())
  (foreach item sup-list
    (setq r-pts (tsp-get-support-item-range-pts item))
    (if (and r-pts (= (length r-pts) 2))
      (progn
        (setq new-r-pts
          (mapcar '(lambda (pt) (list (+ (car pt) dx) (+ (cadr pt) dy) 0.0)) r-pts)
        )
        (setq s-type (car item) idx 0 new-item '())
        (foreach v item
          (if (or (and (wcmatch s-type "*앵커*") (= idx 11))
                  (and (wcmatch s-type "버팀보*") (= idx 13)))
            (setq new-item (append new-item (list new-r-pts)))
            (setq new-item (append new-item (list v)))
          )
          (setq idx (1+ idx))
        )
        (setq out (append out (list new-item)))
      )
      (setq out (append out (list item)))
    )
  )
  out
)

;;;;==========================================================================
;;;; defun tsp-shift-anchor-angles-in-segs : 세그먼트 목록 전체의 앵커 평면각 및 적용 구간 기록 좌표 이동 보정
;;;;==========================================================================
(defun tsp-shift-anchor-angles-in-segs (seg-list dx dy / out seg sup-l)
  (setq out '())
  (foreach seg seg-list
    (if (and (listp seg) (assoc 'SUPPORT-LIST seg) (cdr (assoc 'SUPPORT-LIST seg)))
      (progn
        (setq sup-l (tsp-shift-anchor-angles (cdr (assoc 'SUPPORT-LIST seg)) dx dy))
        (setq sup-l (tsp-shift-support-ranges sup-l dx dy))
        (setq seg (subst (cons 'SUPPORT-LIST sup-l) (assoc 'SUPPORT-LIST seg) seg))
      )
    )
    (setq out (append out (list seg)))
  )
  out
)

;;;;==========================================================================
;;;; defun tsp-build-poly-verts : 내부판정(PIP)용 경계선 정점 리스트 생성
;;;;   - 폐합선 : 각 세그먼트의 V-START 전체 (기존 동작과 동일)
;;;;   - 열린선 : 마지막 세그먼트의 V-END를 추가로 포함
;;;;     (누락 시 PIP 다각형이 '첫점 ~ 마지막 세그먼트 시작점' 대각선으로 닫혀
;;;;      마지막 세그먼트 쪽 내부가 전부 외부로 판정되어 버팀보가 생성되지 않음)
;;;;==========================================================================
(defun tsp-build-poly-verts (boundary-ent / res last-seg v)
  (if (null boundary-ent) (setq boundary-ent *tsp-boundary-ent*))
  (setq res nil)
  (if (and (boundp '*segment-list*) *segment-list* boundary-ent)
    (progn
      (setq res (mapcar '(lambda (x / v)
                           (setq v (trans (cdr (assoc 'V-START x)) boundary-ent 0))
                           (list (car v) (cadr v) 0.0)
                         )
                        *segment-list*))
      (if (not (is-closed-polyline boundary-ent))
        (progn
          (setq last-seg (last *segment-list*))
          (if (and last-seg (cdr (assoc 'V-END last-seg)))
            (progn
              (setq v (trans (cdr (assoc 'V-END last-seg)) boundary-ent 0))
              (setq v (list (car v) (cadr v) 0.0))
              (if (> (distance v (last res)) 1e-4)
                (setq res (append res (list v)))
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
;;;; defun tsp-is-point-inside : 점이 다각형(폴리라인) 내부에 있는지 판별 (Ray-casting 알고리즘)
;;;;==========================================================================
(defun tsp-is-point-inside (pt verts / c i j p1 p2 pt-x pt-y)
  (setq c nil)
  (setq pt-x (car pt) pt-y (cadr pt))
  (setq i 0 j (1- (length verts)))
  (while (< i (length verts))
    (setq p1 (nth i verts) p2 (nth j verts))
    (if (and (not (equal (cadr p1) (cadr p2) 1e-6))
             (or (and (<= (cadr p1) pt-y) (> (cadr p2) pt-y))
                 (and (<= (cadr p2) pt-y) (> (cadr p1) pt-y))))
      (if (< pt-x (+ (car p1) (* (/ (- (car p2) (car p1)) (- (cadr p2) (cadr p1))) (- pt-y (cadr p1)))))
        (setq c (not c))
      )
    )
    (setq j i i (1+ i))
  )
  c
)

;;;;==========================================================================
;;;; [1H 확장] 보조 함수 모음 : 옵션 조회/갱신, 1H 중간말뚝 4방향 배치, 1H 잭 교차 방향
;;;;   - s-brace-opt 11번 인덱스 : 1H 잭 시작 방향 ("0"=위, "1"=아래)
;;;;   - s-brace-opt 12번 인덱스 : 1H 중간말뚝 위치 ("0"=좌상 "1"=우상 "2"=좌하 "3"=우하)
;;;;==========================================================================
(defun tsp-brace-opt-get (opt idx def)
  (if (and opt (listp opt) (> (length opt) idx) (nth idx opt)) (nth idx opt) def)
)

(defun tsp-brace-opt-set (opt idx val)
  (if (not (listp opt)) (setq opt '()))
  (while (<= (length opt) idx)
    (setq opt (append opt (list (if (= (length opt) 10) "" "0"))))
  )
  (update-list-item opt idx val)
)

;; [까치발 삭제] 식별키(key)가 삭제 목록에 있는지 판정 (허용오차 10mm)
(defun tsp-corbel-deleted-p (key del-list / found)
  (setq found nil)
  (if (and key del-list (listp del-list))
    (foreach d del-list
      (if (and (listp d) (numberp (car d)) (< (distance (list (car key) (cadr key)) (list (car d) (cadr d))) 10.0))
        (setq found T)
      )
    )
  )
  found
)

;; [까치발 삭제] mark 이후 생성된 객체를 까치발 레이어로 옮기고 식별키 XDATA 부착 (화타쐐기 연결부용)
(defun tsp-corbel-adopt-ents (mark key / e ed)
  (setq e (if mark (entnext mark) (entnext)))
  (while e
    (setq ed (entget e))
    (if (and ed (not (assoc -3 (entget e '("TSP_CORBEL")))))
      (progn
        (setq ed (subst (cons 8 "_까치발(Corbel)") (assoc 8 ed) ed))
        (setq ed (append ed (list (list -3 (list "TSP_CORBEL" (cons 1010 key))))))
        (entmod ed)
      )
    )
    (setq e (entnext e))
  )
)

;; [까치발 삭제] 삭제 목록 좌표를 경계선 이동량만큼 보정
(defun tsp-shift-deleted-corbels (dx dy / lst)
  (setq lst (vlax-ldata-get "TSP_DICT" "DELETED_CORBELS"))
  (if lst
    (vlax-ldata-put "TSP_DICT" "DELETED_CORBELS"
      (mapcar '(lambda (pt) (list (+ (car pt) dx) (+ (cadr pt) dy) 0.0)) lst))
  )
)

;; [까치발 삭제] 드래그 선택 삭제 : 까치발 레이어 객체만 선택되며,
;;   선택된 객체가 하나라도 속한 까치발(식별키 단위)은 구성 객체 전체를 삭제하고 목록에 기록
(defun tsp-manual-delete-corbel ( / loop ss i ent xd key keys del-list ss-all j e2 xd2 k2 cnt)
  (setq loop T cnt 0)
  (princ "\n[까치발 삭제] 삭제할 까치발을 드래그(또는 클릭)하여 선택하세요. (까치발 레이어 객체만 선택됩니다 / 종료: Enter, ESC)")
  (while loop
    (setq ss (vl-catch-all-apply 'ssget (list '((8 . "_까치발(Corbel)")))))
    (if (or (vl-catch-all-error-p ss) (null ss))
      (setq loop nil)
      (progn
        (setq keys '() i 0)
        (while (< i (sslength ss))
          (setq ent (ssname ss i))
          (setq xd (assoc -3 (entget ent '("TSP_CORBEL"))))
          (if xd
            (progn
              (setq key (cdr (assoc 1010 (cdr (assoc "TSP_CORBEL" (cdr xd))))))
              (if (and key (not (tsp-corbel-deleted-p key keys))) (setq keys (append keys (list key))))
            )
          )
          (setq i (1+ i))
        )
        (if keys
          (progn
            (setq del-list (vlax-ldata-get "TSP_DICT" "DELETED_CORBELS"))
            (if (not del-list) (setq del-list '()))
            (foreach key keys
              (if (not (tsp-corbel-deleted-p key del-list))
                (setq del-list (append del-list (list (list (car key) (cadr key) 0.0))) cnt (1+ cnt))
              )
            )
            (vlax-ldata-put "TSP_DICT" "DELETED_CORBELS" del-list)
            ;; 같은 식별키를 가진 까치발 구성 객체를 화면에서 즉시 제거
            (setq ss-all (ssget "X" '((8 . "_까치발(Corbel)"))))
            (if ss-all
              (progn
                (setq j 0)
                (while (< j (sslength ss-all))
                  (setq e2 (ssname ss-all j))
                  (setq xd2 (assoc -3 (entget e2 '("TSP_CORBEL"))))
                  (if xd2
                    (progn
                      (setq k2 (cdr (assoc 1010 (cdr (assoc "TSP_CORBEL" (cdr xd2))))))
                      (if (and k2 (tsp-corbel-deleted-p k2 keys)) (entdel e2))
                    )
                  )
                  (setq j (1+ j))
                )
              )
            )
            (princ (strcat "\n  -> 까치발 " (itoa (length keys)) "개소 삭제. 계속 선택하거나 Enter로 종료하세요."))
          )
          (princ "\n  -> 선택된 객체에 까치발 정보가 없습니다.")
        )
      )
    )
  )
  (princ (strcat "\n[까치발 삭제] 완료 (이번 작업 삭제 " (itoa cnt) "개소)"))
  (princ)
)

;; [까치발 삭제] 삭제된 까치발 목록 초기화 (도면 전체)
(defun tsp-reset-deleted-corbels ()
  (vlax-ldata-put "TSP_DICT" "DELETED_CORBELS" '())
  (princ "\n[까치발 초기화] 삭제했던 까치발을 모두 복원합니다.")
  (princ)
)

;; [까치발 거리] 단면별 기본값 문자열(m) : 2H = 1.5, 1H = 1.0
(defun tsp-corbel-default-str (type-str)
  (if (= type-str "2H") "1.5" "1.0")
)

;; C.T.C 값(m, 문자열 또는 숫자)을 mm로 변환 (실패 시 5000mm)
(defun tsp-ctc-mm (v / n)
  (setq n (cond ((numberp v) v) ((= (type v) 'STR) (atof v)) (t 0.0)))
  (if (> n 0.0) (* n 1000.0) 5000.0)
)

;; 1H 말뚝 위치 코드 -> 화면 기준 4방향 중 버팀보 축(u)/보강재 축(v) 부호 결정
(defun tsp-1h-post-quadrant (strut-ang pos-code / want-x want-y su sv qx qy d best best-d)
  (setq want-x (if (member pos-code '("1" "3")) 1.0 -1.0))
  (setq want-y (if (member pos-code '("0" "1")) 1.0 -1.0))
  (setq best nil best-d -1e99)
  (foreach su '(1.0 -1.0)
    (foreach sv '(1.0 -1.0)
      (setq qx (+ (* su (cos strut-ang)) (* sv (cos (+ strut-ang (/ pi 2.0))))))
      (setq qy (+ (* su (sin strut-ang)) (* sv (sin (+ strut-ang (/ pi 2.0))))))
      (setq d (+ (* qx want-x) (* qy want-y)))
      (if (> d best-d) (setq best-d d best (list su sv)))
    )
  )
  best
)

;; 1H 말뚝 삽입 정보 (삽입점 회전각 Y스케일) 산출
;;   말뚝 모서리를 (버팀보 측면 x 보강재 플랜지선) 교차점에 밀착시키고,
;;   삽입점(플랜지 중앙)은 말뚝 폭/2 만큼 버팀보 바깥쪽으로 이동
(defun tsp-1h-post-place (p-center strut-ang half-b post-b pos-code / q su sv corner ins)
  (setq q (tsp-1h-post-quadrant strut-ang pos-code))
  (setq su (car q) sv (cadr q))
  (setq corner (polar (polar p-center strut-ang (* su half-b)) (+ strut-ang (/ pi 2.0)) (* sv half-b)))
  (setq ins (polar corner (+ strut-ang (/ pi 2.0)) (* sv (/ post-b 2.0))))
  ;; 블록 자세 : 회전 = 버팀보각-90도 고정, Y스케일 = su (말뚝 몸통이 보강재 반대편으로 향함)
  ;;   (사용자 제공 LIST 실예 4개소: 회전 0도, 보강재 위쪽 +1 / 아래쪽 -1 과 일치)
  (list ins (- strut-ang (/ pi 2.0)) su)
)

;; 이미 작도된 1H 자동 중간말뚝(TSP_POST_1H XDATA 보유)의 위치를 새 코드로 즉시 재배치
(defun tsp-1h-post-reposition (ents pos-code / e ed xd vals pc sa hb pb pp cnt)
  (setq cnt 0)
  (foreach e ents
    (if (and (= (type e) 'ENAME) (entget e))
      (progn
        (setq ed (entget e '("TSP_POST_1H")))
        (setq xd (assoc -3 ed))
        (if (and xd (= (cdr (assoc 0 ed)) "INSERT"))
          (progn
            (setq vals (cdr (car (cdr xd))))
            (setq pc (cdr (nth 0 vals)) sa (cdr (nth 1 vals)) hb (cdr (nth 2 vals)) pb (cdr (nth 3 vals)))
            (setq pp (tsp-1h-post-place (list (car pc) (cadr pc) 0.0) sa hb pb pos-code))
            (setq ed (subst (cons 10 (car pp)) (assoc 10 ed) ed))
            (setq ed (subst (cons 50 (cadr pp)) (assoc 50 ed) ed))
            (setq ed (subst (cons 42 (float (caddr pp))) (assoc 42 ed) ed))
            (entmod ed)
            (entupd e)
            (setq cnt (1+ cnt))
          )
        )
      )
    )
  )
  cnt
)

;;;;==========================================================================
;;;; defun tsp-2h-post-reposition : 이미 작도된 2H 자동 중간말뚝(TSP_POST_2H XDATA 보유)을 보강재 왼쪽(L)/오른쪽(R)으로 즉시 재배치
;;;;==========================================================================
(defun tsp-2h-post-reposition (ents side-code / e ed xd vals pc pdir hb pt-ins post-rot y-sc dist-to-ins ang-to-ins mirror-ang cnt)
  (setq cnt 0)
  (foreach e ents
    (if (and (= (type e) 'ENAME) (entget e))
      (progn
        (setq ed (entget e '("TSP_POST_2H")))
        (setq xd (assoc -3 ed))
        (if (and xd (= (cdr (assoc 0 ed)) "INSERT"))
          (progn
            (setq vals (cdr (car (cdr xd))))
            (setq pc (list (car (cdr (nth 0 vals))) (cadr (cdr (nth 0 vals))) 0.0))
            (setq pdir (cdr (nth 1 vals)))
            (setq hb (cdr (nth 2 vals)))
            (setq pt-ins (polar pc (- pdir (/ pi 2.0)) hb))
            (setq post-rot (+ pdir pi))
            (setq y-sc 1.0)
            (if (= side-code "R")
              (progn
                (setq post-rot (- (* 2.0 pdir) post-rot))
                (setq dist-to-ins (distance pc pt-ins))
                (setq ang-to-ins (angle pc pt-ins))
                (setq mirror-ang (- (* 2.0 pdir) ang-to-ins))
                (setq pt-ins (polar pc mirror-ang dist-to-ins))
                (setq y-sc -1.0)
              )
            )
            (setq ed (subst (cons 10 pt-ins) (assoc 10 ed) ed))
            (setq ed (subst (cons 50 post-rot) (assoc 50 ed) ed))
            (setq ed (subst (cons 42 (float y-sc)) (assoc 42 ed) ed))
            (entmod ed)
            (entupd e)
            (setq cnt (1+ cnt))
          )
        )
      )
    )
  )
  cnt
)

;; 기준 버팀보(base-pt)로부터 몇 번째 버팀보인지(C.T.C 기준) 판정하여 잭을 '위'에 둘지 결정
(defun tsp-1h-jack-want-top (base-pt pt axis-ang ctc start-code / d k start-top)
  (if (or (null ctc) (<= ctc 0.0)) (setq ctc 5000.0))
  (setq d (abs (tsp-project-dist base-pt axis-ang pt)))
  (setq k (fix (+ (/ d ctc) 0.5)))
  (setq start-top (/= start-code "1"))
  (if (= (rem k 2) 1) (not start-top) start-top)
)

;; 평면도 기준 '위' 끝단이 pt2인지 판정 (거의 수평인 버팀보는 왼쪽 끝단을 '위'로 간주)
(defun tsp-1h-jack-at-pt2 (pt1 pt2 want-top / dx dy top-is-pt2)
  (setq dx (- (car pt2) (car pt1)) dy (- (cadr pt2) (cadr pt1)))
  (if (> (abs dy) (* 0.2 (abs dx)))
    (setq top-is-pt2 (> dy 0.0))
    (setq top-is-pt2 (< dx 0.0))
  )
  (if want-top top-is-pt2 (not top-is-pt2))
)

;;;;==========================================================================
;;;;==========================================================================
;;;; [비평행 띠장 접합 & 경사 버팀보 그룹 확장] 기하 연산 및 대화상자 각도 지정 보조 함수
;;;;==========================================================================
(defun tsp-norm-angle-rad (ang)
  (while (<= ang (- pi)) (setq ang (+ ang (* 2.0 pi))))
  (while (> ang pi) (setq ang (- ang (* 2.0 pi))))
  ang
)

(defun tsp-angle-bisector (a1 a2 / vx vy)
  (setq vx (+ (cos a1) (cos a2))
        vy (+ (sin a1) (sin a2)))
  (if (and (< (abs vx) 1e-8) (< (abs vy) 1e-8))
    (+ a1 (/ pi 2.0))
    (atan vy vx)
  )
)

(defun tsp-calc-strut-step-dist (ctc-dist angle-offset-deg / rad cos-v)
  (setq rad (* (if (numberp angle-offset-deg) angle-offset-deg 0.0) (/ pi 180.0)))
  (setq cos-v (abs (cos rad)))
  (if (< cos-v 0.15) (setq cos-v 0.15))
  (/ ctc-dist cos-v)
)

;; 특정 세그먼트의 2D WCS 시작/끝점, 진행각, 굴착면 내측 법선, 띠장 오프셋, 띠장 내측선 양끝점 반환
(defun tsp-get-seg-inward-normal (seg boundary-ent poly-verts / p1 p2 tang n1 n2 mid-pt test1 test2 in-norm orient offset w-p1 w-p2)
  (if (null boundary-ent) (setq boundary-ent *tsp-boundary-ent*))
  (setq p1 (trans (cdr (assoc 'V-START seg)) boundary-ent 0))
  (setq p2 (trans (cdr (assoc 'V-END seg)) boundary-ent 0))
  (setq p1 (list (car p1) (cadr p1) 0.0))
  (setq p2 (list (car p2) (cadr p2) 0.0))
  (setq tang (angle p1 p2))
  (setq n1 (+ tang (/ pi 2.0)))
  (setq n2 (- tang (/ pi 2.0)))
  (setq mid-pt (list (/ (+ (car p1) (car p2)) 2.0) (/ (+ (cadr p1) (cadr p2)) 2.0) 0.0))
  (setq test1 (polar mid-pt n1 50.0))
  (setq test2 (polar mid-pt n2 50.0))
  (cond
    ((and poly-verts (tsp-is-point-inside test1 poly-verts) (not (tsp-is-point-inside test2 poly-verts)))
     (setq in-norm n1))
    ((and poly-verts (tsp-is-point-inside test2 poly-verts) (not (tsp-is-point-inside test1 poly-verts)))
     (setq in-norm n2))
    (t
     (setq orient (if (and (boundp '*tsp-boundary-orient*) (numberp *tsp-boundary-orient*)) *tsp-boundary-orient* 1.0))
     (setq in-norm (+ tang (* orient (/ pi 2.0)))))
  )
  (setq offset (tsp-get-wale-offset seg))
  (setq w-p1 (polar p1 in-norm offset))
  (setq w-p2 (polar p2 in-norm offset))
  (list p1 p2 tang in-norm offset w-p1 w-p2)
)

;;;;==========================================================================
;;;; [띠장 정합] 실제 작도된 띠장 형상 기준 접합 계산
;;;;   1안 : 경계선 추종 확장 세그먼트(= 띠장 작도와 동일 기준)
;;;;   2안 : 도면에 실제 작도된 _띠장(wale) 폴리라인
;;;;==========================================================================
;; 세그먼트 리스트 -> PIP용 정점 리스트
(defun tsp-poly-from-segs (segs boundary-ent / res v last-seg)
  (setq res nil)
  (if (and segs boundary-ent)
    (progn
      (setq res (mapcar '(lambda (x / v)
                           (setq v (trans (cdr (assoc 'V-START x)) boundary-ent 0))
                           (list (car v) (cadr v) 0.0))
                        segs))
      (if (not (is-closed-polyline boundary-ent))
        (progn
          (setq last-seg (last segs))
          (if (and last-seg (cdr (assoc 'V-END last-seg)))
            (progn
              (setq v (trans (cdr (assoc 'V-END last-seg)) boundary-ent 0))
              (setq v (list (car v) (cadr v) 0.0))
              (if (and res (> (distance v (last res)) 1e-4)) (setq res (append res (list v))))
            )
          )
        )
      )
    )
  )
  res
)

;; 다각형 정점 평균(중심)
(defun tsp-poly-center (verts / sx sy n)
  (setq sx 0.0 sy 0.0 n 0)
  (foreach v verts (setq sx (+ sx (car v)) sy (+ sy (cadr v)) n (1+ n)))
  (if (> n 0) (list (/ sx n) (/ sy n) 0.0) nil)
)

;; 점-직선 수직거리
(defun tsp-pt-line-dist (pt p1 p2 / dd)
  (setq dd (distance p1 p2))
  (if (< dd 1e-6) 0.0
    (abs (/ (- (* (- (car p2) (car p1)) (- (cadr pt) (cadr p1)))
               (* (- (cadr p2) (cadr p1)) (- (car pt) (car p1))))
            dd))
  )
)

;; [1안] 경계선 추종 확장 세그먼트 기준 띠장 내측선 목록 (캐시)
(defun tsp-wale-lines-geo (boundary-ent / key segs pol res tot tmp)
  (setq tot 0.0)
  (if (and (boundp '*segment-list*) *segment-list*)
    (foreach sg *segment-list*
      (setq tot (+ tot (if (cdr (assoc 'LENGTH sg)) (cdr (assoc 'LENGTH sg)) 0.0)))
    )
  )
  (setq key (list (if boundary-ent (tsp-get-entity-handle boundary-ent) "")
                  (length *segment-list*) (rtos tot 2 2)
                  (if (or (not (boundp '*tsp-geo-follow-brace-wale*)) *tsp-geo-follow-brace-wale*) T nil)))
  (if (and (boundp '*tsp-wale-lines-cache*) *tsp-wale-lines-cache* (equal key (car *tsp-wale-lines-cache*)))
    (cdr *tsp-wale-lines-cache*)
    (progn
      (setq segs *segment-list*)
      (if (and (or (not (boundp '*tsp-geo-follow-brace-wale*)) *tsp-geo-follow-brace-wale*)
               boundary-ent *segment-list*)
        (progn
          (setq tmp (vl-catch-all-apply 'tsp-expand-segments-by-geometry (list *segment-list* boundary-ent)))
          (if (and (not (vl-catch-all-error-p tmp)) (listp tmp) (car tmp)) (setq segs (car tmp)))
        )
      )
      (if (null segs) (setq segs *segment-list*))
      (setq pol (tsp-poly-from-segs segs boundary-ent))
      (setq res (mapcar '(lambda (s) (tsp-get-seg-inward-normal s boundary-ent pol)) segs))
      (setq res (vl-remove-if '(lambda (x) (null x)) res))
      (setq *tsp-wale-lines-cache* (cons key res))
      res
    )
  )
)

;; [2안] 도면에 실제 작도된 띠장(_띠장) 폴리라인 목록
(defun tsp-wale-lines-dwg (boundary-ent / ss i e ed lay pts p1 p2 res pol mn mp ctr)
  (setq res nil)
  (setq pol (tsp-build-poly-verts boundary-ent))
  (setq ss (ssget "_X" '((0 . "LWPOLYLINE"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq e (ssname ss i) ed (entget e) lay (cdr (assoc 8 ed)))
        (if (and lay (wcmatch lay "*띠장*"))
          (progn
            (setq pts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) ed)))
            (if (>= (length pts) 2)
              (progn
                (setq pts (mapcar '(lambda (p) (list (car p) (cadr p) 0.0)) pts))
                (setq p1 (car pts) p2 (last pts))
                (if (> (distance p1 p2) 1.0)
                  (progn
                    (setq mn (angle p1 p2))
                    (setq mp (list (/ (+ (car p1) (car p2)) 2.0) (/ (+ (cadr p1) (cadr p2)) 2.0) 0.0))
                    ;; 내측 = 굴착부(다각형 내부)를 향하는 법선. 양쪽 모두 내부일 때는 다각형 중심점 방향 우선
                    (setq ctr (tsp-poly-center pol))
                    (if (and ctr (< (distance (polar mp (+ mn (/ pi 2.0)) 50.0) ctr)
                                    (distance (polar mp (- mn (/ pi 2.0)) 50.0) ctr)))
                      (setq mn (+ mn (/ pi 2.0)))
                      (setq mn (- mn (/ pi 2.0)))
                    )
                    (if (and pol (not (tsp-is-point-inside (polar mp mn 50.0) pol)))
                      (setq mn (+ mn pi))
                    )
                    (setq res (append res (list (list p1 p2 (angle p1 p2) mn 0.0 p1 p2))))
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  res
)
