;;; ==========================================================================
;;; 파일명: TSP_5K_DCL_and_Strut.lsp
;;; 구  분: DCL 다이얼로그 UI, 세그먼트 매니저 및 버팀보/앵커/까치발 수동 작도 (Part K)
;;; 역  할: 전체 DCL 생성, 세그먼트 매니저/마법사/대시보드, 지반/범례/벽체/사보강재/지보재 콜백, 수동 버팀보/까치발/연속보강재/앵커편심각/중간말뚝 작도 및 제어 [tsp-get-wale-offset, tsp-strut-preview-restore]
;;; 포함 함수: tsp-get-wale-offset, tsp-strut-preview-restore
;;; --------------------------------------------------------------------------
;;; [안내] 이 파일은 TSP.lsp(메인 로더)에 의해 자동으로 로드되는 하위 모듈입니다.
;;;        코드 수정 시 전체 파일을 합칠 필요 없이 이 파일만 단독 패치하면 됩니다.
;;; ==========================================================================

;;;;==========================================================================
;;;; defun tsp-get-wale-offset : 특정 세그먼트의 띠장 내측면까지의 오프셋 거리 계산 (지보재 설정 띠장 최우선)
;;;;==========================================================================
(defun tsp-get-wale-offset (seg-data / wall-type wale-vals wale-h wale-spec upg-data upg-spec sup-list strut-max-h any-max-h s-wv)
  (setq wall-type (cdr (assoc 'WALL-TYPE seg-data)))
  (if (not wall-type) (setq wall-type "HPILE"))

  ;; 띠장 상향적용(UPGRADE-WALE) 데이터 확인 및 할당
  (setq upg-data (cdr (assoc 'UPGRADE-WALE seg-data)))
  (if (and upg-data (= (car upg-data) "Y") (cadr upg-data))
    (setq upg-spec (cadr upg-data))
    (setq upg-spec nil)
  )

  (if (= wall-type "CIP")
    (setq wale-spec (nth (atoi (if (cdr (assoc 'CIP-WALE-IDX seg-data)) (cdr (assoc 'CIP-WALE-IDX seg-data)) "0")) *tsp-std-wall-list*))
    (progn
      (setq wale-spec (cdr (assoc 'WALE-SPEC seg-data)))
      (if (null wale-spec) (setq wale-spec "H 300x300x10/15"))
    )
  )

  (if upg-spec
    (progn
      (setq wale-vals (parse-h-spec upg-spec))
      (setq wale-h (if wale-vals (nth 0 wale-vals) 300.0))
    )
    (progn
      (setq wale-vals (if (= wale-spec "User-defined") (cdr (assoc 'WALE-CUSTOM seg-data)) (parse-h-spec wale-spec)))
      (setq wale-h (if wale-vals (nth 0 wale-vals) 300.0))
      ;; 지보재(특히 버팀보)에 설정된 띠장 규격이 있으면 해당 띠장 규격을 우선 적용
      (setq sup-list (cdr (assoc 'SUPPORT-LIST seg-data)))
      (if sup-list
        (progn
          (setq strut-max-h 0 any-max-h 0)
          (foreach s-it sup-list
            (setq s-wv (tsp-get-support-item-wale-vals s-it wale-spec (cdr (assoc 'WALE-CUSTOM seg-data))))
            (if (> (nth 0 s-wv) any-max-h) (setq any-max-h (nth 0 s-wv)))
            (if (and (wcmatch (car s-it) "버팀보*") (> (nth 0 s-wv) strut-max-h))
              (setq strut-max-h (nth 0 s-wv))
            )
          )
          (cond
            ((> strut-max-h 0) (setq wale-h (float strut-max-h)))
            ((> any-max-h 0)   (setq wale-h (float any-max-h)))
          )
        )
      )
    )
  )
  wale-h
)

;;;;==========================================================================
;;;; defun support-dialog-callback : 지보재(Strut/Anchor) 관리 대화상자 콜백
;;;;==========================================================================
;;;;==========================================================================
;;;; defun tsp-strut-preview-restore : [직접 작도 취소 시] 작도 전 원래 좌표(orig-pts)로 버팀보 미리보기 재표시
;;;;==========================================================================
(defun tsp-strut-preview-restore (base-supports r-type r-idx r-data orig-pts / real-type p-data p-supports i new-list)
  (if (and orig-pts r-data (> (length r-data) 11)
           (boundp '*tsp-current-seg-idx*) *tsp-current-seg-idx*
           (boundp '*tsp-boundary-ent*) *tsp-boundary-ent*
           (boundp '*tsp-boundary-orient*) *tsp-boundary-orient*)
    (progn
      (setq real-type (if (wcmatch (nth 2 r-data) "*Anchor*") "앵커(Earth Anchor)" "버팀보(Strut)"))
                  (setq p-data (list real-type (nth 1 r-data) (nth 2 r-data) (nth 3 r-data)
                         (nth 4 r-data) (nth 5 r-data) (nth 6 r-data) (nth 7 r-data)
                         orig-pts ;; 작도 전 원래 좌표로 복원
                         (nth 9 r-data) (nth 10 r-data) (nth 11 r-data)
                         (if (> (length r-data) 12) (nth 12 r-data) *tsp-wale-spec*)
                         (if (> (length r-data) 13) (nth 13 r-data) nil)))
      (setq p-supports base-supports)
      (if (= r-type "ADD")
        (setq p-supports (append p-supports (list p-data)))
        (progn
          (setq i 0 new-list '())
          (foreach item p-supports
            (if (= i r-idx)
              (setq new-list (append new-list (list p-data)))
              (setq new-list (append new-list (list item)))
            )
            (setq i (1+ i))
          )
          (setq p-supports new-list)
        )
      )
      (setq *segment-list* (subst (subst (cons 'SUPPORT-LIST p-supports)
                                         (assoc 'SUPPORT-LIST (nth *tsp-current-seg-idx* *segment-list*))
                                         (nth *tsp-current-seg-idx* *segment-list*))
                                  (nth *tsp-current-seg-idx* *segment-list*)
                                  *segment-list*))
      (setq *tsp-last-draw-state* nil)
      (if (boundp 'tsp-redraw-realtime-plan)
        (vl-catch-all-apply 'tsp-redraw-realtime-plan (list *tsp-boundary-ent* *tsp-boundary-orient*))
      )
      (princ "\n[복원] 작도 전 버팀보 배치를 미리보기로 다시 표시했습니다.")
    )
  )
  (princ)
)
