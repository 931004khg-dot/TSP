
(defun C:_TMP_EXTRACT_SCENE_ ( / ss base_pt i ent elist etype outpath fp
                                 visited_blks visited_lays visited_stys visited_ltys
                                 first_ent first_blk first_lay first_sty first_lty
                                 l_el s_el lt_el
                                 _esc _num _dxf_to_json _write_entity_json _collect_blocks _dump_block )
  (vl-load-com)

  ;; 문자열 이스케이프 함수 (JSON 안전 변환)
  (defun _esc (s / idx ch res)
    (if (not s) (setq s ""))
    (setq res "" idx 1)
    (while (<= idx (strlen s))
      (setq ch (substr s idx 1))
      (cond
        ((= ch "\\") (setq res (strcat res "\\\\")))
        ((= ch "\"") (setq res (strcat res "\\\"")))
        ((= ch "\n") (setq res (strcat res "\\n")))
        ((= ch "\r") (setq res (strcat res "\\r")))
        ((= ch "\t") (setq res (strcat res "\\t")))
        (t (setq res (strcat res ch)))
      )
      (setq idx (1+ idx))
    )
    res
  )

  ;; 실수 포맷팅 함수 (배정밀도 15자리 추출 후 불필요한 후행 0 제거 - 수치 오차 0%)
  (defun _num (n / s len last_ch)
    (if (numberp n)
      (if (= (type n) 'INT)
        (itoa n)
        (progn
          (setq s (rtos n 2 15))
          (if (vl-string-search "." s)
            (progn
              (while (and (> (strlen s) 1) (= (substr s (strlen s) 1) "0"))
                (setq s (substr s 1 (1- (strlen s))))
              )
              (if (= (substr s (strlen s) 1) ".")
                (setq s (substr s 1 (1- (strlen s))))
              )
            )
          )
          (if (or (= s "") (= s "-0")) "0" s)
        )
      )
      "0"
    )
  )

  ;; 단일 DXF 그룹을 JSON 배열 [code, type, value] 문자열로 변환
  (defun _dxf_to_json (pair / code val)
    (setq code (car pair)
          val  (cdr pair))
    ;; entmake 시 충돌하는 핸들/엔티티명/리액터/도면공간 코드만 제외 (100번 코드는 보존)
    (if (or (member code '(-1 -2 5 67 330 340 350 360 410))
            (>= code 1000))
      nil
      (cond
        ((= (type val) 'INT)
         (strcat "[" (itoa code) ",\"INT\"," (itoa val) "]"))
        ((= (type val) 'REAL)
         (strcat "[" (itoa code) ",\"REAL\"," (_num val) "]"))
        ((= (type val) 'STR)
         (strcat "[" (itoa code) ",\"STR\",\"" (_esc val) "\"]"))
        ((and (listp val) (>= (length val) 2) (numberp (car val)) (numberp (cadr val)))
         (if (= (length val) 3)
           (strcat "[" (itoa code) ",\"PT3\",[" (_num (car val)) "," (_num (cadr val)) "," (_num (caddr val)) "]]")
           (strcat "[" (itoa code) ",\"PT2\",[" (_num (car val)) "," (_num (cadr val)) "]]")
         ))
        (t nil)
      )
    )
  )

  ;; 엔티티 하나의 전체 DXF 리스트를 JSON 객체로 기록
  (defun _write_entity_json (en is_first / el etype bname eff_name vla_obj sub_en sub_el items jstr sub_items sub_jstr k cnt sk scnt sub_first)
    (setq el (entget en)
          etype (cdr (assoc 0 el)))

    ;; 레이어, 선종류, 텍스트 스타일 수집
    (if (assoc 8 el)
      (if (not (member (strcase (cdr (assoc 8 el))) visited_lays))
        (setq visited_lays (cons (strcase (cdr (assoc 8 el))) visited_lays))
      )
    )
    (if (assoc 6 el)
      (if (not (member (strcase (cdr (assoc 6 el))) visited_ltys))
        (setq visited_ltys (cons (strcase (cdr (assoc 6 el))) visited_ltys))
      )
    )
    (if (assoc 7 el)
      (if (not (member (strcase (cdr (assoc 7 el))) visited_stys))
        (setq visited_stys (cons (strcase (cdr (assoc 7 el))) visited_stys))
      )
    )

    ;; 블록 참조(INSERT)인 경우 블록 정의 수집 및 동적블록 원본명 확인
    (setq eff_name "")
    (if (and (= etype "INSERT") (assoc 2 el))
      (progn
        (setq bname (cdr (assoc 2 el)))
        (_collect_blocks bname)
        (if (= (strcase (substr bname 1 2)) "*U")
          (progn
            (setq vla_obj (vlax-ename->vla-object en))
            (if (vlax-property-available-p vla_obj 'EffectiveName)
              (setq eff_name (vla-get-EffectiveName vla_obj))
              (setq eff_name bname)
            )
          )
          (setq eff_name bname)
        )
      )
    )

    ;; DXF 그룹 필터링
    (setq items '())
    (foreach p el
      ;; DIMENSION의 익명 블록 이름(2번 코드)은 새 도면에서 재생성되도록 제외
      (if (not (and (= etype "DIMENSION") (= (car p) 2)))
        (progn
          (setq jstr (_dxf_to_json p))
          (if jstr (setq items (cons jstr items)))
        )
      )
    )
    (setq items (reverse items))

    (if (not is_first) (write-line "    ," fp))
    (write-line "    {" fp)
    (write-line (strcat "      \"etype\": \"" (_esc etype) "\",") fp)
    (if (/= eff_name "")
      (write-line (strcat "      \"eff_name\": \"" (_esc eff_name) "\",") fp)
    )
    (write-line "      \"dxf\": [" fp)
    (setq k 0 cnt (length items))
    (foreach it items
      (setq k (1+ k))
      (if (< k cnt)
        (write-line (strcat "        " it ",") fp)
        (write-line (strcat "        " it) fp)
      )
    )
    (write-line "      ]," fp)

    ;; 구형 POLYLINE(VERTEX 포함) 또는 ATTRIB 포함 복합 객체(66 그룹코드 = 1) 처리
    (write-line "      \"sub_entities\": [" fp)
    (if (= (cdr (assoc 66 el)) 1)
      (progn
        (setq sub_en (entnext en)
              sub_first T)
        (while (and sub_en
                    (setq sub_el (entget sub_en))
                    (/= (cdr (assoc 0 sub_el)) "SEQEND"))
          (setq sub_items '())
          (foreach sp sub_el
            (setq sub_jstr (_dxf_to_json sp))
            (if sub_jstr (setq sub_items (cons sub_jstr sub_items)))
          )
          (setq sub_items (reverse sub_items))
          (if (not sub_first) (write-line "        ," fp))
          (setq sub_first nil)
          (write-line "        [" fp)
          (setq sk 0 scnt (length sub_items))
          (foreach sit sub_items
            (setq sk (1+ sk))
            (if (< sk scnt)
              (write-line (strcat "          " sit ",") fp)
              (write-line (strcat "          " sit) fp)
            )
          )
          (write-line "        ]" fp)
          (setq sub_en (entnext sub_en))
        )
      )
    )
    (write-line "      ]" fp)
    (write-line "    }" fp)
  )

  ;; 블록 정의 및 중첩 블록 재귀 탐색 함수
  (defun _collect_blocks (blk_name / tbl_el sub_en el etype)
    (if (and blk_name
             (not (member (strcase blk_name) visited_blks))
             (setq tbl_el (tblsearch "BLOCK" blk_name)))
      (progn
        (setq visited_blks (cons (strcase blk_name) visited_blks))
        (setq sub_en (cdr (assoc -2 tbl_el)))
        (while sub_en
          (setq el (entget sub_en)
                etype (cdr (assoc 0 el)))
          (if (and (= etype "INSERT") (assoc 2 el))
            (_collect_blocks (cdr (assoc 2 el)))
          )
          (if (= (cdr (assoc 66 el)) 1)
            (while (and (setq sub_en (entnext sub_en))
                        (/= (cdr (assoc 0 (entget sub_en))) "SEQEND")))
          )
          (setq sub_en (entnext sub_en))
        )
      )
    )
  )

  ;; 블록 하나를 JSON으로 출력
  (defun _dump_block (blk_name is_first / tbl_el bpt bflags sub_en el first_b_ent)
    (setq tbl_el (tblsearch "BLOCK" blk_name))
    (if tbl_el
      (progn
        (setq bpt (cdr (assoc 10 tbl_el))
              bflags (cdr (assoc 70 tbl_el))
              sub_en (cdr (assoc -2 tbl_el)))
        (if (not bpt) (setq bpt '(0.0 0.0 0.0)))
        (if (not bflags) (setq bflags 0))

        (if (not is_first) (write-line "    ," fp))
        (write-line "    {" fp)
        (write-line (strcat "      \"name\": \"" (_esc (cdr (assoc 2 tbl_el))) "\",") fp)
        (write-line (strcat "      \"flags\": " (itoa bflags) ",") fp)
        (write-line (strcat "      \"base_point\": [" (_num (car bpt)) "," (_num (cadr bpt)) "," (_num (caddr bpt)) "],") fp)
        (write-line "      \"entities\": [" fp)

        (setq first_b_ent T)
        (while sub_en
          (setq el (entget sub_en))
          (_write_entity_json sub_en first_b_ent)
          (setq first_b_ent nil)
          (if (= (cdr (assoc 66 el)) 1)
            (while (and (setq sub_en (entnext sub_en))
                        (/= (cdr (assoc 0 (entget sub_en))) "SEQEND")))
          )
          (setq sub_en (entnext sub_en))
        )

        (write-line "      ]" fp)
        (write-line "    }" fp)
      )
    )
  )

  ;; ==================== 메인 실행부 ====================
  (setq outpath "C:/LISP/TSP/tmp_cad_scene_extract.json")
  (princ "\n==================================================================")
  (princ "\n[도면 데이터 추출] 1단계: 데이터화할 도면 객체들을 선택하세요 (드래그/클릭 후 Enter): ")
  (setq ss (ssget))
  (if (and ss (> (sslength ss) 0))
    (progn
      (initget 1)
      (princ "\n[도면 데이터 추출] 2단계: 기준점(Base Point)을 클릭하세요: ")
      (setq base_pt (getpoint))
      (if base_pt
        (progn
          ;; UCS 좌표를 WCS 절대좌표로 변환
          (setq base_pt (trans base_pt 1 0))
          (setq visited_blks '()
                visited_lays '()
                visited_stys '()
                visited_ltys '())

          (setq fp (open outpath "w"))
          (if fp
            (progn
              (write-line "{" fp)
              (write-line "  \"status\": \"OK\"," fp)
              (write-line (strcat "  \"base_point\": [" (_num (car base_pt)) "," (_num (cadr base_pt)) "," (_num (caddr base_pt)) "],") fp)
              (write-line (strcat "  \"entity_count\": " (itoa (sslength ss)) ",") fp)

              ;; 1. 선택된 도면 객체들 기록
              (write-line "  \"entities\": [" fp)
              (setq first_ent T
                    i 0)
              (while (< i (sslength ss))
                (setq ent (ssname ss i))
                (_write_entity_json ent first_ent)
                (setq first_ent nil
                      i (1+ i))
              )
              (write-line "  ]," fp)

              ;; 2. 참조된 블록 정의 기록 (하위 중첩 블록부터 순서대로)
              (write-line "  \"blocks\": [" fp)
              (setq first_blk T)
              (foreach bn visited_blks
                (_dump_block bn first_blk)
                (setq first_blk nil)
              )
              (write-line "  ]," fp)

              ;; 3. 사용된 레이어 테이블 정보 기록
              (write-line "  \"layers\": [" fp)
              (setq first_lay T)
              (foreach ln visited_lays
                (setq l_el (tblsearch "LAYER" ln))
                (if l_el
                  (progn
                    (if (assoc 6 l_el)
                      (if (not (member (strcase (cdr (assoc 6 l_el))) visited_ltys))
                        (setq visited_ltys (cons (strcase (cdr (assoc 6 l_el))) visited_ltys))
                      )
                    )
                    (if (not first_lay) (write-line "    ," fp))
                    (setq first_lay nil)
                    (write-line (strcat "    {\"name\": \"" (_esc (cdr (assoc 2 l_el))) "\","
                                        " \"color\": " (itoa (abs (cdr (assoc 62 l_el)))) ","
                                        " \"linetype\": \"" (_esc (cdr (assoc 6 l_el))) "\"}") fp)
                  )
                )
              )
              (write-line "  ]," fp)

              ;; 4. 사용된 텍스트 스타일 테이블 정보 기록
              (write-line "  \"styles\": [" fp)
              (setq first_sty T)
              (foreach sn visited_stys
                (setq s_el (tblsearch "STYLE" sn))
                (if s_el
                  (progn
                    (if (not first_sty) (write-line "    ," fp))
                    (setq first_sty nil)
                    (write-line (strcat "    {\"name\": \"" (_esc (cdr (assoc 2 s_el))) "\","
                                        " \"font\": \"" (_esc (cdr (assoc 3 s_el))) "\","
                                        " \"bigfont\": \"" (_esc (if (assoc 4 s_el) (cdr (assoc 4 s_el)) "")) "\","
                                        " \"width\": " (_num (if (assoc 41 s_el) (cdr (assoc 41 s_el)) 1.0)) "}") fp)
                  )
                )
              )
              (write-line "  ]" fp)
              (write-line "}" fp)
              (close fp)
              (princ (strcat "\n[완료] 총 " (itoa (sslength ss)) "개의 객체가 기준점 상대좌표 데이터로 정밀 추출되었습니다!"))
            )
          )
        )
        (progn
          (setq fp (open outpath "w"))
          (if fp
            (progn
              (write-line "{\"status\": \"CANCEL\", \"message\": \"기준점(Base Point) 지정이 취소되었습니다.\"}" fp)
              (close fp)
            )
          )
        )
      )
    )
    (progn
      (setq fp (open outpath "w"))
      (if fp
        (progn
          (write-line "{\"status\": \"CANCEL\", \"message\": \"객체 선택이 취소되었거나 선택된 객체가 없습니다.\"}" fp)
          (close fp)
        )
      )
      (princ "\n[취소] 객체 선택이 취소되었습니다.")
    )
  )
  (princ)
)
(C:_TMP_EXTRACT_SCENE_)
