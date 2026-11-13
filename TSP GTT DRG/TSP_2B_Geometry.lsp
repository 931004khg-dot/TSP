;;; ==========================================================================
;;; 파일명: TSP_2B_Geometry.lsp
;;; 구  분: 기하학 좌표 계산, 모서리 편집 및 세그먼트 초기화 (Part B)
;;; 역  할: 폴리라인 정점 추출, CW/CCW 방향 및 오목/볼록 판별, 코너 H-Pile 회전각 계산, 대화형 모서리 추가/삭제 편집(D/A), 세그먼트 리스트 초기화 [init-segment-data]
;;; 포함 함수: init-segment-data
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;; ==========================================================================
;;; [SECTION 3-1] 세그먼트 데이터 초기화
;;; ==========================================================================

;;;;==========================================================================
;;;; defun init-segment-data : 상위에서 정렬된 vertices를 받아 세그먼트 초기화
;;;;==========================================================================
(defun init-segment-data (boundary-ent boundary-orient override-vertices / vertices is-closed num-segs i pt1 pt2 pt0 seg-angle seg-len seg-data new-list old-data use-existing is-convex default-brace-data ang-v1 ang-v2 diff prev-seg-len min-len old-brace-entry old-brace-val old-upg-entry old-first-seg old-pt1 new-pt1 delta-x delta-y translated-deleted translated-added translated-support j shifted-sup translated-seg-sup sup-list new-seg updated-segment-list pt new-pt rec sup item)
  (if (not (boundp '*segment-list*)) (setq *segment-list* nil))
  (if override-vertices
    (setq vertices override-vertices)
    (setq vertices (extract-vertices boundary-ent))
  )
  (setq is-closed (is-closed-polyline boundary-ent))
  (setq num-segs (if is-closed (length vertices) (1- (length vertices))))
  (setq use-existing nil)
  (if (and *segment-list* (= (length *segment-list*) num-segs))
    (setq use-existing T)
  )
  (if use-existing
    (progn
      (princ "\n[알림] 이전 작업 데이터를 불러옵니다.")

      ;; [도면 내 객체 복사/이동 시 좌표 보정 로직]
      (setq old-first-seg (nth 0 *segment-list*))
      (setq old-pt1 (cdr (assoc 'V-START old-first-seg)))
      (setq new-pt1 (nth 0 vertices))
      (setq delta-x (- (car new-pt1) (car old-pt1)))
      (setq delta-y (- (cadr new-pt1) (cadr old-pt1)))

      ;; 0.1 이상의 이동이 발생했을 경우에만 보정 실행
      (if (or (> (abs delta-x) 0.1) (> (abs delta-y) 0.1))
        (progn
          (princ (strcat "\n[시스템] 경계선 이동 감지 (dx: " (rtos delta-x 2 2) ", dy: " (rtos delta-y 2 2) "). 수동 말뚝 및 버팀보 좌표를 동기화합니다."))

          ;; 1. 삭제된 말뚝 보정
          (if (and (boundp '*tsp-deleted-post-points*) *tsp-deleted-post-points*)
            (progn
              (setq translated-deleted '())
              (foreach pt *tsp-deleted-post-points*
                (setq translated-deleted (cons (list (+ (car pt) delta-x) (+ (cadr pt) delta-y) 0.0) translated-deleted))
              )
              (setq *tsp-deleted-post-points* (reverse translated-deleted))
            )
          )
          (tsp-shift-deleted-corbels delta-x delta-y) ;; [까치발 삭제] 삭제 목록 좌표 보정

          ;; 2. 추가된 말뚝 보정
          (if (and (boundp '*tsp-added-post-points*) *tsp-added-post-points*)
            (progn
              (setq translated-added '())
              (foreach rec *tsp-added-post-points*
                (setq pt (car rec))
                (setq new-pt (list (+ (car pt) delta-x) (+ (cadr pt) delta-y) 0.0))
                (setq translated-added (cons (cons new-pt (cdr rec)) translated-added))
              )
              (setq *tsp-added-post-points* (reverse translated-added))
            )
          )

          ;; 3. 글로벌 버팀보 좌표 보정 (*tsp-support-list*) - 타입 검사(listp) 추가
          (if (and (boundp '*tsp-support-list*) *tsp-support-list*)
            (progn
              (setq translated-support '())
              (foreach sup *tsp-support-list*
                (setq shifted-sup '())
                (setq j 0)
                (foreach item sup
                  (if (= j 8)
                    (if (and item (listp item)) ; item이 리스트(좌표 배열)일 때만 mapcar 실행
                      (setq shifted-sup (cons (mapcar (function (lambda (pt)
                                                                        (if (listp pt) ; pt도 정상적인 좌표 리스트인지 검증
                                                                          (list (+ (car pt) delta-x) (+ (cadr pt) delta-y) (if (caddr pt) (caddr pt) 0.0))
                                                                          pt
                                                                        )
                                                                      )) item) shifted-sup))
                      (setq shifted-sup (cons item shifted-sup)) ; 문자열이면 원본 유지
                    )
                    (setq shifted-sup (cons item shifted-sup))
                  )
                  (setq j (1+ j))
                )
                (setq translated-support (cons (reverse shifted-sup) translated-support))
              )
              (setq *tsp-support-list* (reverse translated-support))
            )
          )

          ;; 4. 세그먼트 내부 SUPPORT-LIST 보정 (*segment-list*) - 타입 검사(listp) 추가
          (setq updated-segment-list '())
          (foreach seg *segment-list*
            (setq sup-list (cdr (assoc 'SUPPORT-LIST seg)))
            (if sup-list
              (progn
                (setq translated-seg-sup '())
                (foreach sup sup-list
                  (setq shifted-sup '())
                  (setq j 0)
                  (foreach item sup
                    (if (= j 8)
                      (if (and item (listp item))
                        (setq shifted-sup (cons (mapcar (function (lambda (pt)
                                                                        (if (listp pt)
                                                                          (list (+ (car pt) delta-x) (+ (cadr pt) delta-y) (if (caddr pt) (caddr pt) 0.0))
                                                                          pt
                                                                        )
                                                                      )) item) shifted-sup))
                        (setq shifted-sup (cons item shifted-sup))
                      )
                      (setq shifted-sup (cons item shifted-sup))
                    )
                    (setq j (1+ j))
                  )
                  (setq translated-seg-sup (cons (reverse shifted-sup) translated-seg-sup))
                )
                (setq new-seg (subst (cons 'SUPPORT-LIST (reverse translated-seg-sup)) (assoc 'SUPPORT-LIST seg) seg))
                (setq updated-segment-list (cons new-seg updated-segment-list))
              )
              (setq updated-segment-list (cons seg updated-segment-list))
            )
          )
          (setq *segment-list* (reverse updated-segment-list))
          ;; 앵커 평면각 기록(기준점 절대좌표)도 동일 이동량으로 보정
          (setq *segment-list* (tsp-shift-anchor-angles-in-segs *segment-list* delta-x delta-y))
          (if (and (boundp '*tsp-support-list*) *tsp-support-list*)
            (setq *tsp-support-list* (tsp-shift-anchor-angles *tsp-support-list* delta-x delta-y))
          )
        )
      )
    )
    (princ (strcat "\n[STEP 1] " (itoa num-segs) "개의 세그먼트 데이터 초기화 중..."))
  )

  (setq new-list '())
  (setq i 0)
  (while (< i num-segs)
    (setq pt1 (nth i vertices))
    (setq pt2 (nth (if is-closed (rem (+ i 1) num-segs) (+ i 1)) vertices))
    (setq pt0 (if is-closed
                (nth (if (= i 0) (1- num-segs) (1- i)) vertices)
                (if (> i 0) (nth (1- i) vertices) nil)))
    (setq is-convex nil)
    (if (and pt0 pt2)
      (progn
        (if (is-corner-convex pt0 pt1 pt2 boundary-orient) (setq is-convex T))
        (setq ang-v1 (angle pt1 pt0))
        (setq ang-v2 (angle pt1 pt2))
        (setq diff (abs (- ang-v1 ang-v2)))
        (if (> diff pi) (setq diff (- (* 2.0 pi) diff)))
        (if (>= diff (* 160.0 (/ pi 180.0)))
          (setq is-convex nil)
        )
      )
    )
    (setq seg-len (distance pt1 pt2))
    (setq seg-angle (angle pt1 pt2))
    (setq prev-seg-len (if pt0 (distance pt0 pt1) seg-len))
    (setq min-len (min seg-len prev-seg-len))
    (if (< min-len 1000.0)
      (setq is-convex nil)
    )
    (setq default-brace-data
      (if is-convex
        (list "Y" nil "1" "0" "0.0" "1" "50")
        (list "N" nil)
      )
    )
    (if use-existing
      (progn
        (setq old-data (nth i *segment-list*))
        (setq seg-data (subst (cons 'V-START pt1) (assoc 'V-START old-data) old-data))
        (setq seg-data (subst (cons 'V-END pt2) (assoc 'V-END seg-data) seg-data))
        (setq seg-data (subst (cons 'ANGLE seg-angle) (assoc 'ANGLE seg-data) seg-data))
        (setq seg-data (subst (cons 'LENGTH seg-len) (assoc 'LENGTH seg-data) seg-data))
        (setq seg-data (subst (cons 'ID (1+ i)) (assoc 'ID seg-data) seg-data))
        (setq old-brace-entry (assoc 'CORNER-BRACE seg-data))
        (if old-brace-entry
          (progn
            (setq old-brace-val (cdr old-brace-entry))
            (if is-convex
              (if (= (car old-brace-val) "N")
                (setq seg-data (subst (cons 'CORNER-BRACE default-brace-data) old-brace-entry seg-data))
              )
              (setq seg-data (subst (cons 'CORNER-BRACE default-brace-data) old-brace-entry seg-data))
            )
          )
          (setq seg-data (append seg-data (list (cons 'CORNER-BRACE default-brace-data))))
        )
        (setq old-upg-entry (assoc 'UPGRADE-WALE seg-data))
        (if (not old-upg-entry)
          (setq seg-data (append seg-data (list (cons 'UPGRADE-WALE nil))))
        )
        (if (not (assoc 'WALL-TYPE seg-data))
          (setq seg-data (append seg-data (list (cons 'WALL-TYPE "HPILE"))))
        )
        (if (not (assoc 'CIP-MAX-DEPTH seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-MAX-DEPTH "10.0"))))
        )
        (if (not (assoc 'CIP-EMBED-DEPTH seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-EMBED-DEPTH "3.0"))))
        )
        (if (not (assoc 'CIP-HPILE-IDX seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-HPILE-IDX "2"))))
        )
        (if (not (assoc 'CIP-WALE-IDX seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-WALE-IDX "2"))))
        )
        (if (not (assoc 'CIP-DIA seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-DIA "500"))))
        )
        (if (not (assoc 'CIP-MODE-IDX seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-MODE-IDX "0"))))
        )
        (if (not (assoc 'CIP-OVERLAP seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-OVERLAP "0"))))
        )
        (if (not (assoc 'CIP-EQUAL-DIVIDE seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-EQUAL-DIVIDE "0"))))
        )
        (if (not (assoc 'CIP-INTERVAL-IDX seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-INTERVAL-IDX "2"))))
        )
        (if (not (assoc 'CIP-ANCHOR-N seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-ANCHOR-N (itoa (1+ (atoi (cdr (assoc 'CIP-INTERVAL-IDX seg-data)))))))))
        )
        (if (not (assoc 'CIP-GROUTING-IDX seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-GROUTING-IDX "0"))))
        )
        (if (not (assoc 'CIP-GROUTING-SPACE-IDX seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-GROUTING-SPACE-IDX "0"))))
        )
        (if (not (assoc 'CIP-GROUTING-SPACE-CUSTOM seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-GROUTING-SPACE-CUSTOM ""))))
        )
        (if (not (assoc 'CIP-GROUTING-OFFSET-IDX seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-GROUTING-OFFSET-IDX "0"))))
        )
        (if (not (assoc 'CIP-GROUTING-OFFSET-CUSTOM seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-GROUTING-OFFSET-CUSTOM ""))))
        )
        (if (not (assoc 'SUB-ZONES seg-data))
          (setq seg-data (append seg-data (list (cons 'SUB-ZONES nil))))
        )
      )
      (progn
        (setq seg-data (list
          (cons 'ID (1+ i))
          (cons 'NAME (strcat "Seg-" (itoa (1+ i))))
          (cons 'IS-DEFINED nil)
          (cons 'SECTION-DRAW nil)
          (cons 'SOIL-DATA nil)
          (cons 'WALL-SPEC *tsp-hpile-spec*)
          (cons 'WALL-CUSTOM '(300 300 10 15))
          (cons 'WALE-SPEC *tsp-wale-spec*)
          (cons 'WALE-CUSTOM '(300 300 10 15))
          (cons 'CTC 2.0)
          (cons 'TIMBER-THICKNESS 60)
          (cons 'MAX-DEPTH 10.0)
          (cons 'EMBED-DEPTH 3.0)
          (cons 'WATER-CHK "0")
          (cons 'WATER-DEPTH 0.0)
          (cons 'SHOTCRETE-ENABLE "1")
          (cons 'SHOTCRETE-CHK "0")
          (cons 'SHOTCRETE-LAYER "")
          (cons 'SHOTCRETE-THICK 0.0)
          (cons 'SUPPORT-LIST '())
          (cons 'CORNER-BRACE default-brace-data)
          (cons 'UPGRADE-WALE nil)
          (cons 'WALL-TYPE "HPILE")
          (cons 'CIP-MAX-DEPTH "10.0")
          (cons 'CIP-EMBED-DEPTH "3.0")
          (cons 'CIP-HPILE-IDX "2")
          (cons 'CIP-WALE-IDX "2")
          (cons 'CIP-DIA "500")
          (cons 'CIP-MODE-IDX "0")
          (cons 'CIP-OVERLAP "0")
          (cons 'CIP-EQUAL-DIVIDE "0")
          (cons 'CIP-INTERVAL-IDX "2")
          (cons 'CIP-ANCHOR-N "3")
          (cons 'CIP-GROUTING-IDX "0")
          (cons 'CIP-GROUTING-SPACE-IDX "0")
          (cons 'CIP-GROUTING-SPACE-CUSTOM "")
          (cons 'CIP-GROUTING-OFFSET-IDX "0")
          (cons 'CIP-GROUTING-OFFSET-CUSTOM "")
          (cons 'V-START pt1)
          (cons 'V-END pt2)
          (cons 'ANGLE seg-angle)
          (cons 'LENGTH seg-len)
        ))
      )
    )
    ;; [Phase 1] cons 최적화
    (setq new-list (cons seg-data new-list))
    (setq i (1+ i))
  )
  (setq *segment-list* (reverse new-list))
  (princ "\n[완료] 세그먼트 리스트 준비됨.")
  (princ (strcat "\n - 총 세그먼트 수: " (itoa (length *segment-list*))))
)
