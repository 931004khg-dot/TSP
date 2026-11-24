# -*- coding: utf-8 -*-
"""
AutoCAD 독립형 블록 라이브러리 매니저 & LISP 코드 생성기 (cad_block_manager.py)
- CAD 도면 내 블록 정밀 추출 (중첩 블록 및 모든 2D/기하학 객체 DXF 데이터 완벽 보존)
- 블록 속성(이름, 삽입 레이어/색상/선타입, 내부 객체 레이어/색상/선타입/텍스트 스타일) 일괄 조정 및 원본 유지
- 2D 형상 미리보기 캔버스 (Canvas Preview)
- CAD에 직접 붙여넣기 (삽입점 지정 후 SC / ROTATE 명령 즉시 연동 지원)
- 다중 선택(체크박스) 기반 독립형 AutoLISP(.lsp) 파일 자동 생성 (CP949 인코딩 및 유니코드 공백 정화)
"""

import os
import sys
import json
import math
import time
import shutil
import tempfile
import traceback
import re
import tkinter as tk
from tkinter import ttk, messagebox, filedialog

# COM 연동을 위한 pywin32 임포트 확인
try:
    import win32com.client
    import pythoncom
    HAS_WIN32COM = True
except ImportError:
    HAS_WIN32COM = False


# ==============================================================================
# 1. AutoCAD 내장 LISP 추출 엔진 템플릿
#    (블록 내부의 모든 객체와 중첩 블록, 레이어, 텍스트 스타일의 순수 DXF 데이터를 추출)
# ==============================================================================
LISP_EXTRACTOR_TEMPLATE = r"""
(defun C:_TMP_EXTRACT_BLK_ ( / sel ent elist bname eff_name outpath fp
                               visited_blks visited_lays visited_stys
                               first_blk first_lay first_sty l_el s_el
                               _esc _num _dxf_to_json _write_entity_json _collect_blocks _dump_block )
  (vl-load-com)

  ;; 문자열 이스케이프 함수 (JSON 안전 변환)
  (defun _esc (s / i ch res)
    (if (not s) (setq s ""))
    (setq res "" i 1)
    (while (<= i (strlen s))
      (setq ch (substr s i 1))
      (cond
        ((= ch "\\") (setq res (strcat res "\\\\")))
        ((= ch "\"") (setq res (strcat res "\\\"")))
        ((= ch "\n") (setq res (strcat res "\\n")))
        ((= ch "\r") (setq res (strcat res "\\r")))
        ((= ch "\t") (setq res (strcat res "\\t")))
        (t (setq res (strcat res ch)))
      )
      (setq i (1+ i))
    )
    res
  )

  ;; 실수 포맷팅 함수
  (defun _num (n)
    (if (numberp n)
      (if (= (type n) 'INT)
        (itoa n)
        (rtos n 2 8)
      )
      "0"
    )
  )

  ;; 단일 DXF 그룹을 JSON 배열 [code, type, value] 문자열로 변환
  ;; (주의: 100번 Subclass Marker는 ELLIPSE, HATCH, SPLINE, LWPOLYLINE 등의 entmake에 필수이므로 반드시 보존!)
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
  (defun _write_entity_json (en is_first / el etype sub_en sub_el items jstr sub_items sub_jstr i cnt si scnt sub_first)
    (setq el (entget en)
          etype (cdr (assoc 0 el)))
    ;; 레이어 및 텍스트 스타일 수집
    (if (assoc 8 el)
      (if (not (member (strcase (cdr (assoc 8 el))) visited_lays))
        (setq visited_lays (cons (strcase (cdr (assoc 8 el))) visited_lays))
      )
    )
    (if (assoc 7 el)
      (if (not (member (strcase (cdr (assoc 7 el))) visited_stys))
        (setq visited_stys (cons (strcase (cdr (assoc 7 el))) visited_stys))
      )
    )
    ;; 중첩 블록(INSERT)인 경우 대상 블록 이름 수집
    (if (and (= etype "INSERT") (assoc 2 el))
      (_collect_blocks (cdr (assoc 2 el)))
    )

    ;; DXF 그룹 필터링
    (setq items '())
    (foreach p el
      (setq jstr (_dxf_to_json p))
      (if jstr (setq items (cons jstr items)))
    )
    (setq items (reverse items))

    (if (not is_first) (write-line "," fp))
    (write-line "        {" fp)
    (write-line (strcat "          \"etype\": \"" (_esc etype) "\",") fp)
    (write-line "          \"dxf\": [" fp)
    (setq i 0 cnt (length items))
    (foreach it items
      (setq i (1+ i))
      (if (< i cnt)
        (write-line (strcat "            " it ",") fp)
        (write-line (strcat "            " it) fp)
      )
    )
    (write-line "          ]," fp)

    ;; 구형 POLYLINE 또는 ATTRIB 포함 복합 객체(66 그룹코드 = 1) 처리
    (write-line "          \"sub_entities\": [" fp)
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
          (if (not sub_first) (write-line "            ," fp))
          (setq sub_first nil)
          (write-line "            [" fp)
          (setq si 0 scnt (length sub_items))
          (foreach sit sub_items
            (setq si (1+ si))
            (if (< si scnt)
              (write-line (strcat "              " sit ",") fp)
              (write-line (strcat "              " sit) fp)
            )
          )
          (write-line "            ]" fp)
          (setq sub_en (entnext sub_en))
        )
      )
    )
    (write-line "          ]" fp)
    (write-line "        }" fp)
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
          ;; POLYLINE 내부 VERTEX 건너뛰기
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
  (defun _dump_block (blk_name is_first / tbl_el bpt bflags sub_en el first_ent)
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

        (setq first_ent T)
        (while sub_en
          (setq el (entget sub_en))
          (_write_entity_json sub_en first_ent)
          (setq first_ent nil)
          ;; 복합 객체(POLYLINE 등)인 경우 SEQEND까지 이동
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

  ;; 메인 실행부
  (setq outpath "__OUTPATH__")
  (princ "\n=======================================================")
  (princ "\n[블록 매니저] 저장할 블록(INSERT) 객체를 클릭하세요: ")
  (setq sel (entsel))
  (if (and sel (setq ent (car sel)) (= (cdr (assoc 0 (entget ent))) "INSERT"))
    (progn
      (setq elist (entget ent)
            bname (cdr (assoc 2 elist)))
      ;; 동적 블록(익명 블록 *U...)인 경우 원본 블록 이름 확인 시도
      (if (= (strcase (substr bname 1 2)) "*U")
        (if (vlax-property-available-p (vlax-ename->vla-object ent) 'EffectiveName)
          (setq eff_name (vla-get-EffectiveName (vlax-ename->vla-object ent)))
          (setq eff_name bname)
        )
        (setq eff_name bname)
      )

      (setq visited_blks '()
            visited_lays '()
            visited_stys '())

      ;; 참조된 블록 수집 (자신 및 중첩 블록)
      (_collect_blocks bname)
      ;; visited_blks는 역순(가장 하위 중첩 블록이 뒤에 추가됨 -> reverse하면 메인 블록이 맨 앞)
      ;; 생성 시에는 하위 블록부터 먼저 정의되어야 하므로 순서 유지

      (setq fp (open outpath "w"))
      (if fp
        (progn
          (write-line "{" fp)
          (write-line "  \"status\": \"OK\"," fp)
          (write-line (strcat "  \"main_block\": \"" (_esc eff_name) "\",") fp)
          (write-line (strcat "  \"raw_block_name\": \"" (_esc bname) "\",") fp)
          (write-line (strcat "  \"insert_layer\": \"" (_esc (cdr (assoc 8 elist))) "\",") fp)
          (write-line (strcat "  \"insert_color\": " (if (assoc 62 elist) (itoa (cdr (assoc 62 elist))) "256") ",") fp)
          (write-line (strcat "  \"insert_linetype\": \"" (if (assoc 6 elist) (_esc (cdr (assoc 6 elist))) "ByLayer") "\",") fp)

          ;; 블록 정의 리스트 기록 (하위 블록부터 메인 블록 순서로 기록)
          (write-line "  \"blocks\": [" fp)
          (setq first_blk T)
          (foreach bn visited_blks
            (_dump_block bn first_blk)
            (setq first_blk nil)
          )
          (write-line "  ]," fp)

          ;; 사용된 레이어 테이블 정보 기록
          (write-line "  \"layers\": [" fp)
          (setq first_lay T)
          (foreach ln visited_lays
            (setq l_el (tblsearch "LAYER" ln))
            (if l_el
              (progn
                (if (not first_lay) (write-line "    ," fp))
                (setq first_lay nil)
                (write-line (strcat "    {\"name\": \"" (_esc (cdr (assoc 2 l_el))) "\","
                                    " \"color\": " (itoa (abs (cdr (assoc 62 l_el)))) ","
                                    " \"linetype\": \"" (_esc (cdr (assoc 6 l_el))) "\"}") fp)
              )
            )
          )
          (write-line "  ]," fp)

          ;; 사용된 텍스트 스타일 테이블 정보 기록
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
          (princ (strcat "\n[블록 매니저] '" eff_name "' 블록 데이터 추출 완료!"))
        )
      )
    )
    (progn
      (setq fp (open outpath "w"))
      (if fp
        (progn
          (write-line "{\"status\": \"CANCEL\", \"message\": \"블록(INSERT) 객체가 선택되지 않았거나 취소되었습니다.\"}" fp)
          (close fp)
        )
      )
      (princ "\n[블록 매니저] 블록 선택이 취소되었거나 유효한 블록이 아닙니다.")
    )
  )
  (princ)
)
(C:_TMP_EXTRACT_BLK_)
"""


# ==============================================================================
# 2. 색상 매핑 및 유틸리티 함수
# ==============================================================================
ACI_COLORS = {
    "ByBlock (0)": 0,
    "ByLayer (256)": 256,
    "1 (Red - 빨강)": 1,
    "2 (Yellow - 노랑)": 2,
    "3 (Green - 초록)": 3,
    "4 (Cyan - 하늘)": 4,
    "5 (Blue - 파랑)": 5,
    "6 (Magenta - 자홍)": 6,
    "7 (White/Black - 흰색)": 7,
    "8 (Dark Gray - 진회색)": 8,
    "9 (Light Gray - 연회색)": 9,
    "30 (Orange - 주황)": 30,
    "250 (Gray 250)": 250,
    "252 (Gray 252)": 252,
    "254 (Gray 254)": 254,
}

ACI_TO_HEX = {
    0: "#FFFFFF",
    1: "#FF0000",   # Red
    2: "#FFFF00",   # Yellow
    3: "#00FF00",   # Green
    4: "#00FFFF",   # Cyan
    5: "#0066FF",   # Blue
    6: "#FF00FF",   # Magenta
    7: "#FFFFFF",   # White
    8: "#808080",   # Dark Gray
    9: "#C0C0C0",   # Light Gray
    10: "#FF0000",
    11: "#FF7F7F",  # 11번 색상 (Light Red / Coral Pink)
    12: "#CC0000",
    13: "#CC6666",
    14: "#990000",
    20: "#FF3F00",
    21: "#FF9F7F",
    30: "#FF7F00",  # Orange
    31: "#FFBF7F",
    40: "#FFBF00",
    41: "#FFDF7F",
    50: "#FFFF00",
    51: "#FFFF7F",
    60: "#BFFF00",
    70: "#7FFF00",
    80: "#3FFF00",
    90: "#00FF00",
    100: "#00FF3F",
    110: "#00FF7F",
    120: "#00FFBF",
    130: "#00FFFF",
    131: "#7FFFFF",
    140: "#00BFFF",
    141: "#7FDFFF",
    150: "#007FFF",
    160: "#003FFF",
    170: "#0000FF",
    180: "#3F00FF",
    190: "#7F00FF",
    200: "#BF00FF",
    210: "#FF00FF",
    220: "#FF00BF",
    230: "#FF007F",
    240: "#FF003F",
    250: "#4C4C4C",
    251: "#686868",
    252: "#848484",
    253: "#A0A0A0",
    254: "#BCBCBC",
    255: "#E0E0E0",
    256: "#FFFFFF",
}


def aci_to_hex_color(aci: int) -> str:
    """AutoCAD 색상 인덱스(ACI 0~256)를 화면 표시용 HEX 색상 코드로 변환합니다."""
    aci = int(aci)
    if aci in ACI_TO_HEX:
        return ACI_TO_HEX[aci]
    if 10 <= aci <= 249:
        # 10~249 구간의 표준 색상환 근사 계산
        hue_idx = (aci - 10) // 10
        sub_idx = (aci - 10) % 10
        hue = (hue_idx * 15.0) % 360.0
        import colorsys
        lightness = 0.72 if (sub_idx % 2 == 1) else 0.50
        if sub_idx >= 4:
            lightness *= 0.75
        r, g, b = colorsys.hls_to_rgb(hue / 360.0, lightness, 0.9)
        return f"#{int(r*255):02X}{int(g*255):02X}{int(b*255):02X}"
    return "#00E5FF"


def sanitize_for_cp949(text: str) -> str:
    """
    리습(.lsp) 파일을 ANSI(CP949)로 인코딩할 때 발생할 수 있는
    유니코드 특수 공백 및 비호환 문자를 일반 공백으로 안전하게 정화합니다.
    """
    text = re.sub(r'[\u00A0\u1680\u180E\u2000-\u200B\u202F\u205F\u3000\uFEFF]', ' ', text)
    cleaned_chars = []
    for ch in text:
        try:
            ch.encode('cp949')
            cleaned_chars.append(ch)
        except UnicodeEncodeError:
            cleaned_chars.append(' ')
    return "".join(cleaned_chars)


def safe_lisp_str(val: str) -> str:
    """AutoLISP 문자열 리터럴 내부에 들어갈 텍스트를 이스케이프합니다."""
    if val is None:
        return ""
    s = str(val)
    s = s.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n").replace("\r", "\\r")
    return s


def sanitize_symbol_name(name: str) -> str:
    """LISP 함수명으로 사용 가능하도록 공백 및 특수문자를 정화합니다."""
    s = re.sub(r'[\s\(\)\[\]\{\}\"\'\;\,\.\/\\]+', '_', name.strip())
    return s if s else "BLOCK"


# ==============================================================================
# 3. AutoLISP 코드 생성 엔진 (블록 데이터 + 사용자 오버라이드 설정 -> 순수 LISP 코드)
# ==============================================================================
class LispCodeGenerator:
    """
    저장된 블록 JSON 데이터와 사용자가 설정한 속성(오버라이드 또는 원본 유지)을 바탕으로
    독립 실행 가능한 AutoLISP (entmake 기반) 코드를 생성합니다.
    """

    # 엔티티 타입별 필수 DXF 100 서브클래스 마커 (구버전 추출 데이터 호환용 폴백)
    DEFAULT_SUBCLASS_MAP = {
        "LINE": ["AcDbEntity", "AcDbLine"],
        "CIRCLE": ["AcDbEntity", "AcDbCircle"],
        "ARC": ["AcDbEntity", "AcDbCircle", "AcDbArc"],
        "ELLIPSE": ["AcDbEntity", "AcDbEllipse"],
        "LWPOLYLINE": ["AcDbEntity", "AcDbPolyline"],
        "SPLINE": ["AcDbEntity", "AcDbSpline"],
        "TEXT": ["AcDbEntity", "AcDbText", "AcDbText"],
        "MTEXT": ["AcDbEntity", "AcDbMText"],
        "HATCH": ["AcDbEntity", "AcDbHatch"],
        "SOLID": ["AcDbEntity", "AcDbTrace"],
        "POINT": ["AcDbEntity", "AcDbPoint"],
        "INSERT": ["AcDbEntity", "AcDbBlockReference"],
    }

    @staticmethod
    def resolve_color_value(setting_str: str, original_val: int) -> int:
        if not setting_str or setting_str.startswith("[원본"):
            return original_val
        if setting_str in ACI_COLORS:
            return ACI_COLORS[setting_str]
        m = re.search(r'(\d+)', setting_str)
        if m:
            val = int(m.group(1))
            if 0 <= val <= 256:
                return val
        return original_val

    @staticmethod
    def build_block_lisp_code(block_record: dict, include_commands: bool = True) -> str:
        """
        단일 블록 레코드(중첩 블록 포함)를 생성하고 배치하는 AutoLISP 전체 함수 코드를 반환합니다.
        """
        settings = block_record.get("settings", {})
        use_original_all = settings.get("use_original_all", False)

        # 1. 최종 적용할 블록 이름
        orig_main_name = block_record.get("main_block", "MY_BLOCK")
        raw_main_name = block_record.get("raw_block_name", orig_main_name)
        target_main_name = orig_main_name if use_original_all else settings.get("block_name", orig_main_name).strip()
        if not target_main_name:
            target_main_name = orig_main_name

        func_sym = sanitize_symbol_name(target_main_name)

        # 2. 삽입(INSERT) 기준 속성 결정
        orig_ins_layer = block_record.get("insert_layer", "0")
        orig_ins_color = block_record.get("insert_color", 256)
        orig_ins_ltype = block_record.get("insert_linetype", "ByLayer")

        if use_original_all:
            ins_layer = orig_ins_layer
            ins_color = orig_ins_color
            ins_ltype = orig_ins_ltype
            ent_layer_mode = "[원본 유지 (Original)]"
            ent_color_mode = "[원본 유지 (Original)]"
            ent_ltype_mode = "[원본 유지 (Original)]"
            ent_style_mode = "[원본 유지 (Original)]"
            font_shx = "romans.shx"
            bigfont_shx = "whgtxt.shx"
        else:
            ins_layer_sel = settings.get("insert_layer", "[원본 유지 (Original)]").strip()
            ins_layer = orig_ins_layer if ins_layer_sel.startswith("[원본") or not ins_layer_sel else ins_layer_sel

            ins_color_sel = settings.get("insert_color", "[원본 유지 (Original)]").strip()
            ins_color = LispCodeGenerator.resolve_color_value(ins_color_sel, orig_ins_color)

            ins_ltype_sel = settings.get("insert_linetype", "[원본 유지 (Original)]").strip()
            ins_ltype = orig_ins_ltype if ins_ltype_sel.startswith("[원본") or not ins_ltype_sel else ins_ltype_sel

            ent_layer_mode = settings.get("entity_layer", "[원본 유지 (Original)]").strip()
            ent_color_mode = settings.get("entity_color", "[원본 유지 (Original)]").strip()
            ent_ltype_mode = settings.get("entity_linetype", "[원본 유지 (Original)]").strip()
            ent_style_mode = settings.get("entity_textstyle", "[원본 유지 (Original)]").strip()
            font_shx = settings.get("textstyle_font", "romans.shx").strip() or "romans.shx"
            bigfont_shx = settings.get("textstyle_bigfont", "whgtxt.shx").strip()

        lines = []
        lines.append(f";; ============================================================================")
        lines.append(f";; 블록명: {target_main_name} (원본: {orig_main_name})")
        lines.append(f";; ============================================================================")
        lines.append(f"(defun _CREATE_BLK_{func_sym} ( / )")

        # 3. 필요한 선종류(LTYPE) 및 레이어(LAYER) 테이블 자동 생성 로직
        layers_to_ensure = {}
        for lay in block_record.get("layers", []):
            layers_to_ensure[lay["name"].upper()] = dict(lay)

        if ins_layer and ins_layer != "0" and ins_layer.upper() not in layers_to_ensure:
            layers_to_ensure[ins_layer.upper()] = {"name": ins_layer, "color": 7, "linetype": "Continuous"}
        if ent_layer_mode and not ent_layer_mode.startswith("[원본") and ent_layer_mode != "0":
            if ent_layer_mode.upper() not in layers_to_ensure:
                layers_to_ensure[ent_layer_mode.upper()] = {"name": ent_layer_mode, "color": 7, "linetype": "Continuous"}

        if layers_to_ensure:
            lines.append("  ;; [1] 필수 레이어 테이블 확인 및 자동 생성")
            for _, lay_info in layers_to_ensure.items():
                lname = safe_lisp_str(lay_info["name"])
                lcol = int(lay_info.get("color", 7))
                lltype = safe_lisp_str(lay_info.get("linetype", "Continuous"))
                if lname.upper() == "0":
                    continue
                lines.append(f'  (if (not (tblsearch "LAYER" "{lname}"))')
                lines.append(f'    (entmake (list \'(0 . "LAYER") \'(100 . "AcDbSymbolTableRecord") \'(100 . "AcDbLayerTableRecord")')
                lines.append(f'                   \'(2 . "{lname}") \'(70 . 0) \'(62 . {lcol})')
                lines.append(f'                   (cons 6 (if (tblsearch "LTYPE" "{lltype}") "{lltype}" "Continuous"))))')
                lines.append('  )')

        # 4. 필요한 텍스트 스타일 테이블 자동 생성 로직
        styles_to_ensure = {}
        for sty in block_record.get("styles", []):
            styles_to_ensure[sty["name"].upper()] = dict(sty)

        if ent_style_mode and not ent_style_mode.startswith("[원본"):
            styles_to_ensure[ent_style_mode.upper()] = {
                "name": ent_style_mode,
                "font": font_shx,
                "bigfont": bigfont_shx,
                "width": 1.0
            }

        if styles_to_ensure:
            lines.append("  ;; [2] 필수 문자 스타일(STYLE) 테이블 확인 및 자동 생성")
            for _, sty_info in styles_to_ensure.items():
                sname = safe_lisp_str(sty_info["name"])
                sfont = safe_lisp_str(sty_info.get("font", "romans.shx") or "romans.shx")
                sbig = safe_lisp_str(sty_info.get("bigfont", ""))
                swid = float(sty_info.get("width", 1.0))
                if sname.upper() == "STANDARD":
                    continue
                lines.append(f'  (if (not (tblsearch "STYLE" "{sname}"))')
                lines.append(f'    (entmake (list \'(0 . "STYLE") \'(100 . "AcDbSymbolTableRecord") \'(100 . "AcDbTextStyleTableRecord")')
                lines.append(f'                   \'(2 . "{sname}") \'(70 . 0) \'(40 . 0.0) \'(41 . {swid:.4f}) \'(50 . 0.0) \'(71 . 0)')
                lines.append(f'                   \'(3 . "{sfont}") \'(4 . "{sbig}")))')
                lines.append('  )')

        # 5. 각 블록(중첩 블록 -> 메인 블록 순서) entmake 생성
        blocks_list = block_record.get("blocks", [])
        lines.append("  ;; [3] 블록 및 내부 구성 객체(Entity) 생성")
        for blk in blocks_list:
            bname_orig = blk.get("name", "")
            if bname_orig.upper() == raw_main_name.upper() or bname_orig.upper() == orig_main_name.upper():
                bname_final = target_main_name
            else:
                bname_final = bname_orig

            bname_escaped = safe_lisp_str(bname_final)
            bpt = blk.get("base_point", [0.0, 0.0, 0.0])
            bflags = int(blk.get("flags", 0))
            bflags = bflags & (~1) & (~4) & (~32)

            lines.append(f'  ;; --- 블록 정의 시작: {bname_escaped} ---')
            lines.append(f'  (entmake (list \'(0 . "BLOCK") \'(100 . "AcDbEntity") \'(8 . "0") \'(100 . "AcDbBlockBegin") \'(2 . "{bname_escaped}") \'(70 . {bflags}) \'(10 {bpt[0]:.8f} {bpt[1]:.8f} {bpt[2]:.8f})))')

            for ent in blk.get("entities", []):
                dxf_expr = LispCodeGenerator._build_entity_dxf_list_expr(
                    ent,
                    ent_layer_mode=ent_layer_mode,
                    ent_color_mode=ent_color_mode,
                    ent_ltype_mode=ent_ltype_mode,
                    ent_style_mode=ent_style_mode
                )
                lines.append(f'  (entmake {dxf_expr})')

                sub_ents = ent.get("sub_entities", [])
                if sub_ents:
                    for sub_dxf in sub_ents:
                        sub_ent_dict = {"etype": "VERTEX", "dxf": sub_dxf}
                        sub_expr = LispCodeGenerator._build_entity_dxf_list_expr(
                            sub_ent_dict,
                            ent_layer_mode=ent_layer_mode,
                            ent_color_mode=ent_color_mode,
                            ent_ltype_mode=ent_ltype_mode,
                            ent_style_mode=ent_style_mode
                        )
                        lines.append(f'  (entmake {sub_expr})')
                    lines.append('  (entmake \'((0 . "SEQEND") (100 . "AcDbEntity") (8 . "0")))')

            lines.append('  (entmake \'((0 . "ENDBLK") (100 . "AcDbEntity") (8 . "0") (100 . "AcDbBlockEnd")))')

        lines.append(f'  "{safe_lisp_str(target_main_name)}"')
        lines.append(")")

        # 6. 사용자 삽입 명령어 생성 (삽입점 지정 후 SC / ROTATE 연동 포함)
        if include_commands:
            ins_layer_esc = safe_lisp_str(ins_layer)
            ins_ltype_esc = safe_lisp_str(ins_ltype)

            lines.append("")
            lines.append(f";; [명령어] IB_{func_sym} : '{target_main_name}' 블록을 생성하고 마우스로 배치 후 SC/ROTATE 즉시 조정")
            lines.append(f"(defun C:IB_{func_sym} ( / bname pt last_ent kw )")
            lines.append(f"  (vl-load-com)")
            lines.append(f"  (setq bname (_CREATE_BLK_{func_sym}))")
            lines.append(f"  (if (not (tblsearch \"BLOCK\" bname))")
            lines.append(f"    (princ (strcat \"\\n[오류] '\" bname \"' 블록 정의 생성에 실패했습니다. (원본 블록을 다시 추출해 주세요)\"))")
            lines.append(f"    (progn")
            lines.append(f'      (princ (strcat "\\n[블록 삽입] \'" bname "\' 삽입 기준점을 지정하세요: "))')
            lines.append(f"      (if (setq pt (getpoint))")
            lines.append(f"        (progn")
            lines.append(f"          ;; 블록 인스턴스(INSERT) 생성")
            lines.append(f"          (entmake (list '(0 . \"INSERT\")")
            lines.append(f"                         '(100 . \"AcDbEntity\")")
            lines.append(f"                         (cons 8 \"{ins_layer_esc}\")")
            lines.append(f"                         (cons 62 {ins_color})")
            if ins_ltype_esc.upper() != "BYLAYER":
                lines.append(f"                         (cons 6 (if (tblsearch \"LTYPE\" \"{ins_ltype_esc}\") \"{ins_ltype_esc}\" \"ByLayer\"))")
            lines.append(f"                         '(100 . \"AcDbBlockReference\")")
            lines.append(f"                         (cons 2 bname)")
            lines.append(f"                         (cons 10 (trans pt 1 0))")
            lines.append(f"                         '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) '(50 . 0.0)))")
            lines.append(f"          (setq last_ent (entlast))")
            lines.append(f"          (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) acActiveViewport)")
            lines.append(f"          (redraw last_ent 3)")
            lines.append(f"          ;; 삽입 직후 SC(축척) 및 ROTATE(회전) 명령어 연속 실행 루프")
            lines.append(f"          (setq kw \"LOOP\")")
            lines.append(f"          (while kw")
            lines.append(f"            (initget \"SC Scale S RO Rotate R X Exit\")")
            lines.append(f'            (setq kw (getkword "\\n[블록 조정] 명령 선택 [축척(SC)/회전(RO)/완료(Enter)]: "))')
            lines.append(f"            (cond")
            lines.append(f"              ((member kw '(\"SC\" \"Scale\" \"S\"))")
            lines.append(f'               (princ "\\n>> 축척(SCALE) 배율을 지정하거나 마우스로 조정하세요...")')
            lines.append(f"               (command \"_.SCALE\" last_ent \"\" pt pause)")
            lines.append(f"              )")
            lines.append(f"              ((member kw '(\"RO\" \"Rotate\" \"R\"))")
            lines.append(f'               (princ "\\n>> 회전(ROTATE) 각도를 지정하거나 마우스로 조정하세요...")')
            lines.append(f"               (command \"_.ROTATE\" last_ent \"\" pt pause)")
            lines.append(f"              )")
            lines.append(f"              (t (setq kw nil))")
            lines.append(f"            )")
            lines.append(f"          )")
            lines.append(f"          (redraw last_ent 4)")
            lines.append(f'          (princ (strcat "\\n[완료] \'" bname "\' 블록 배치가 완료되었습니다."))')
            lines.append(f"        )")
            lines.append(f"      )")
            lines.append(f"    )")
            lines.append(f"  )")
            lines.append(f"  (princ)")
            lines.append(f")")

        return "\n".join(lines)

    @staticmethod
    def _build_entity_dxf_list_expr(
        ent: dict,
        ent_layer_mode: str,
        ent_color_mode: str,
        ent_ltype_mode: str,
        ent_style_mode: str
    ) -> str:
        """
        단일 엔티티의 DXF 배열을 받아 사용자 오버라이드 설정을 반영한 뒤
        AutoLISP '(list ...)' 표현식 문자열로 변환합니다.
        (그룹코드 100 서브클래스 마커를 완벽히 보존하며, 구버전 추출 데이터는 자동 복원합니다.)
        """
        etype = ent.get("etype", "")
        raw_dxf = ent.get("dxf", [])

        override_layer = (ent_layer_mode and not ent_layer_mode.startswith("[원본"))
        override_color = (ent_color_mode and not ent_color_mode.startswith("[원본"))
        override_ltype = (ent_ltype_mode and not ent_ltype_mode.startswith("[원본"))
        override_style = (ent_style_mode and not ent_style_mode.startswith("[원본"))

        target_color_int = LispCodeGenerator.resolve_color_value(ent_color_mode, 256)

        has_100 = any(item[0] == 100 for item in raw_dxf if len(item) >= 3)
        has_color = any(item[0] == 62 for item in raw_dxf if len(item) >= 3)
        has_ltype = any(item[0] == 6 for item in raw_dxf if len(item) >= 3)
        has_style = any(item[0] == 7 for item in raw_dxf if len(item) >= 3)

        # HATCH 연관(Associative) 플래그 강제 비연관화 (외부 객체 핸들 참조 오류 방지)
        is_hatch = (etype == "HATCH")

        lisp_pairs = []
        inserted_entity_props = False

        for item in raw_dxf:
            if len(item) < 3:
                continue
            code, vtype, val = item[0], item[1], item[2]

            # entmake 시 충돌하는 핸들/시스템 포인터 코드만 제거 (100번은 절대 제거하지 않음!)
            if code in (-1, -2, 5, 67, 330, 340, 350, 360, 410):
                continue
            if etype == "DIMENSION" and code == 2:
                continue
            # HATCH의 연관 경계 객체 개수(97)가 있으면 0으로 강제 설정
            if is_hatch and code == 71:
                val = 0
                vtype = "INT"
            elif is_hatch and code == 97:
                val = 0
                vtype = "INT"

            # 1) 레이어(8) 오버라이드
            if code == 8 and override_layer:
                val = ent_layer_mode
                vtype = "STR"

            # 2) 색상(62) 오버라이드
            elif code == 62 and override_color:
                val = target_color_int
                vtype = "INT"
            elif code == 420 and override_color:
                continue

            # 3) 선종류(6) 오버라이드
            elif code == 6 and override_ltype:
                val = ent_ltype_mode
                vtype = "STR"

            # 4) 문자 스타일(7) 오버라이드
            elif code == 7 and override_style:
                val = ent_style_mode
                vtype = "STR"

            # LISP 구문 변환
            if vtype == "INT":
                lisp_pairs.append(f"'({code} . {int(val)})")
            elif vtype == "REAL":
                lisp_pairs.append(f"'({code} . {float(val):.8f})")
            elif vtype == "STR":
                lisp_pairs.append(f'\'({code} . "{safe_lisp_str(val)}")')
            elif vtype == "PT2":
                lisp_pairs.append(f"'({code} {float(val[0]):.8f} {float(val[1]):.8f})")
            elif vtype == "PT3":
                lisp_pairs.append(f"'({code} {float(val[0]):.8f} {float(val[1]):.8f} {float(val[2]):.8f})")

            # 구버전 추출 데이터(100번이 누락된 데이터)를 위한 자동 서브클래스 마커 복원 및 오버라이드 속성 삽입
            if code == 8 and not inserted_entity_props:
                inserted_entity_props = True
                if override_color and not has_color:
                    lisp_pairs.append(f"'(62 . {target_color_int})")
                if override_ltype and not has_ltype:
                    lisp_pairs.append(f'\'(6 . "{safe_lisp_str(ent_ltype_mode)}")')
                if override_style and not has_style and etype in ("TEXT", "MTEXT", "ATTDEF", "ATTRIB"):
                    lisp_pairs.append(f'\'(7 . "{safe_lisp_str(ent_style_mode)}")')

                # 만약 100번 코드가 없던 구버전 데이터라면 AcDbEntity 및 하위 클래스 마커 자동 주입
                if not has_100 and etype in LispCodeGenerator.DEFAULT_SUBCLASS_MAP:
                    subclasses = LispCodeGenerator.DEFAULT_SUBCLASS_MAP[etype]
                    # 맨 앞 (0 . ETYPE) 바로 뒤에 (100 . "AcDbEntity") 삽입
                    if len(lisp_pairs) >= 1:
                        lisp_pairs.insert(1, '\'(100 . "AcDbEntity")')
                    if len(subclasses) >= 2:
                        lisp_pairs.append(f'\'(100 . "{subclasses[1]}")')

        return "(list " + " ".join(lisp_pairs) + ")"


# ==============================================================================
# 4. 메인 GUI 애플리케이션 클래스
# ==============================================================================
class BlockManagerApp:
    def __init__(self, root: tk.Tk):
        self.root = root
        self.root.title("AutoCAD 독립형 블록 라이브러리 매니저 & LISP 생성기")
        self.root.geometry("1220x820")
        self.root.minsize(1050, 720)

        # 데이터베이스 파일 경로
        self.base_dir = os.path.dirname(os.path.abspath(__file__))
        self.db_path = os.path.join(self.base_dir, "blocks_db.json")

        # 메모리 내 블록 데이터 구조: { block_id: block_record_dict }
        self.blocks_db = {}
        self.current_block_id = None

        # 데이터베이스 로드
        self.load_database()

        # UI 구성
        self._build_ui()
        self._refresh_tree()

    # --------------------------------------------------------------------------
    # 데이터베이스 입출력 (자동 .bak 백업 포함)
    # --------------------------------------------------------------------------
    def load_database(self):
        if os.path.exists(self.db_path):
            try:
                with open(self.db_path, "r", encoding="utf-8") as f:
                    data = json.load(f)
                    if isinstance(data, dict):
                        self.blocks_db = data
            except Exception as e:
                messagebox.showerror("데이터 로드 오류", f"blocks_db.json 파일을 읽는 중 오류가 발생했습니다:\n{e}")
                self.blocks_db = {}
        else:
            self.blocks_db = {}

    def save_database(self):
        try:
            # 수정 전 기존 파일이 있으면 .bak 백업 자동 생성
            if os.path.exists(self.db_path):
                bak_path = self.db_path + ".bak"
                shutil.copy2(self.db_path, bak_path)

            with open(self.db_path, "w", encoding="utf-8") as f:
                json.dump(self.blocks_db, f, ensure_ascii=False, indent=2)
        except Exception as e:
            messagebox.showerror("데이터 저장 오류", f"blocks_db.json 저장 중 오류가 발생했습니다:\n{e}")

    # --------------------------------------------------------------------------
    # UI 레이아웃 설계
    # --------------------------------------------------------------------------
    def _build_ui(self):
        style = ttk.Style()
        if "vista" in style.theme_names():
            style.theme_use("vista")

        # 상단 툴바 프레임
        top_frame = ttk.Frame(self.root, padding=(10, 8))
        top_frame.pack(fill=tk.X)

        btn_extract = tk.Button(
            top_frame,
            text="➕ CAD에서 블록 선택하여 가져오기",
            bg="#0066CC", fg="white", font=("맑은 고딕", 10, "bold"),
            padx=12, pady=5, cursor="hand2",
            command=self.extract_block_from_cad
        )
        btn_extract.pack(side=tk.LEFT, padx=(0, 8))

        btn_insert = tk.Button(
            top_frame,
            text="📌 선택한 블록 CAD에 직접 붙여넣기 (SC / ROTATE 연동)",
            bg="#107C41", fg="white", font=("맑은 고딕", 10, "bold"),
            padx=12, pady=5, cursor="hand2",
            command=self.insert_selected_block_to_cad
        )
        btn_insert.pack(side=tk.LEFT, padx=(0, 8))

        btn_export_lisp = tk.Button(
            top_frame,
            text="📜 체크(☑)한 블록들을 리습(.lsp) 파일로 내보내기",
            bg="#5C2D91", fg="white", font=("맑은 고딕", 10, "bold"),
            padx=12, pady=5, cursor="hand2",
            command=self.export_checked_blocks_to_lisp
        )
        btn_export_lisp.pack(side=tk.LEFT, padx=(0, 8))

        btn_delete = tk.Button(
            top_frame,
            text="🗑 블록 삭제",
            bg="#D83B01", fg="white", font=("맑은 고딕", 9, "bold"),
            padx=10, pady=5, cursor="hand2",
            command=self.delete_selected_block
        )
        btn_delete.pack(side=tk.RIGHT)

        ttk.Separator(self.root, orient=tk.HORIZONTAL).pack(fill=tk.X, padx=10)

        # 메인 분할 창 (좌측: 블록 목록 / 우측: 미리보기 + 속성 편집기)
        paned = ttk.PanedWindow(self.root, orient=tk.HORIZONTAL)
        paned.pack(fill=tk.BOTH, expand=True, padx=10, pady=8)

        # ==================== [좌측 패널: 저장된 블록 목록] ====================
        left_frame = ttk.LabelFrame(paned, text=" 📂 저장된 블록 라이브러리 목록 (체크박스 다중선택 지원) ", padding=8)
        paned.add(left_frame, weight=4)

        # 다중 선택 제어 버튼 줄
        sel_bar = ttk.Frame(left_frame)
        sel_bar.pack(fill=tk.X, pady=(0, 6))

        ttk.Button(sel_bar, text="☑ 전체 체크", command=lambda: self._set_all_checks(True)).pack(side=tk.LEFT, padx=(0, 4))
        ttk.Button(sel_bar, text="☐ 전체 해제", command=lambda: self._set_all_checks(False)).pack(side=tk.LEFT, padx=(0, 4))
        ttk.Button(sel_bar, text="☑ 선택 항목만 체크", command=lambda: self._set_selected_checks(True)).pack(side=tk.LEFT, padx=(14, 4))
        ttk.Button(sel_bar, text="☐ 선택 항목만 해제", command=lambda: self._set_selected_checks(False)).pack(side=tk.LEFT, padx=(0, 4))
        ttk.Label(sel_bar, text="※ Ctrl+클릭 / Shift+클릭으로 여러 항목을 선택한 뒤 일괄 체크/해제/삭제할 수 있습니다.", foreground="#555555").pack(side=tk.LEFT, padx=6)

        # Treeview 테이블 구성
        cols = ("checked", "block_name", "orig_name", "ent_count", "ins_layer")
        self.tree = ttk.Treeview(left_frame, columns=cols, show="headings", selectmode="extended")  # Ctrl/Shift 다중 선택 지원
        self.tree.heading("checked", text="선택")
        self.tree.heading("block_name", text="적용 블록 이름")
        self.tree.heading("orig_name", text="원본 이름")
        self.tree.heading("ent_count", text="객체 수")
        self.tree.heading("ins_layer", text="삽입 레이어")

        self.tree.column("checked", width=50, anchor=tk.CENTER, stretch=False)
        self.tree.column("block_name", width=150, anchor=tk.W)
        self.tree.column("orig_name", width=120, anchor=tk.W)
        self.tree.column("ent_count", width=65, anchor=tk.CENTER, stretch=False)
        self.tree.column("ins_layer", width=100, anchor=tk.CENTER)

        tree_scroll = ttk.Scrollbar(left_frame, orient=tk.VERTICAL, command=self.tree.yview)
        self.tree.configure(yscrollcommand=tree_scroll.set)

        self.tree.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        tree_scroll.pack(side=tk.RIGHT, fill=tk.Y)

        # 트리 이벤트 바인딩 (클릭 시 체크박스 토글 및 우측 패널 로드)
        self.tree.bind("<Button-1>", self._on_tree_click)
        self.tree.bind("<<TreeviewSelect>>", self._on_tree_select)

        # ==================== [우측 패널: 미리보기 + 속성 설정] ====================
        right_frame = ttk.Frame(paned)
        paned.add(right_frame, weight=6)

        # 1) 상단: 2D 형상 미리보기 캔버스
        preview_group = ttk.LabelFrame(right_frame, text=" 🔍 2D 블록 형상 미리보기 (노란 십자선 = 블록 삽입 기준점) ", padding=6)
        preview_group.pack(fill=tk.BOTH, expand=True, pady=(0, 8))

        self.canvas = tk.Canvas(preview_group, bg="#181A1F", height=250, highlightthickness=1, highlightbackground="#444444")
        self.canvas.pack(fill=tk.BOTH, expand=True)
        self.canvas.bind("<Configure>", lambda e: self._draw_preview())

        # 블록 내부 구성 요약 라벨
        self.lbl_summary = ttk.Label(preview_group, text="선택된 블록 없음", foreground="#0055AA", font=("맑은 고딕", 9, "bold"))
        self.lbl_summary.pack(anchor=tk.W, pady=(4, 0))

        # 2) 하단: 블록 및 내부 객체 속성 일괄 조정 패널
        edit_group = ttk.LabelFrame(right_frame, text=" ⚙ 선택한 블록 데이터 및 속성 설정 (방식 1: 일괄 변환 & 원본 그대로 사용) ", padding=10)
        edit_group.pack(fill=tk.X)

        # [원본 데이터 100% 그대로 사용] 체크박스 및 복원 버튼
        orig_bar = ttk.Frame(edit_group)
        orig_bar.pack(fill=tk.X, pady=(0, 8))

        self.var_use_original_all = tk.BooleanVar(value=False)
        chk_orig = ttk.Checkbutton(
            orig_bar,
            text="체크 시 아래 변경값을 무시하고 [선택한 기존 블록의 원본 데이터 100% 그대로 사용]",
            variable=self.var_use_original_all,
            command=self._on_toggle_use_original
        )
        chk_orig.pack(side=tk.LEFT)

        ttk.Button(
            orig_bar,
            text="↺ 모든 항목 [원본 유지]로 초기화",
            command=self._reset_settings_to_original
        ).pack(side=tk.RIGHT)

        ttk.Separator(edit_group, orient=tk.HORIZONTAL).pack(fill=tk.X, pady=6)

        # 입력 폼 그리드
        form = ttk.Frame(edit_group)
        form.pack(fill=tk.X)
        form.columnconfigure(1, weight=1)
        form.columnconfigure(3, weight=1)

        color_choices = ["[원본 유지 (Original)]"] + list(ACI_COLORS.keys())
        ltype_choices = ["[원본 유지 (Original)]", "ByLayer", "ByBlock", "Continuous", "DASHED", "HIDDEN", "CENTER", "PHANTOM", "DOT"]

        # --- 섹션 A: 블록 자체(INSERT) 기본 설정 ---
        ttk.Label(form, text="■ [1] 블록 기본 설정 (도면에 삽입될 때의 블록 자체 속성)", font=("맑은 고딕", 9, "bold"), foreground="#333333").grid(
            row=0, column=0, columnspan=4, sticky=tk.W, pady=(2, 6)
        )

        ttk.Label(form, text="블록 이름(Name):").grid(row=1, column=0, sticky=tk.W, padx=4, pady=4)
        self.var_blk_name = tk.StringVar()
        self.ent_blk_name = ttk.Entry(form, textvariable=self.var_blk_name)
        self.ent_blk_name.grid(row=1, column=1, sticky=tk.EW, padx=4, pady=4)

        ttk.Label(form, text="삽입 레이어(Layer):").grid(row=1, column=2, sticky=tk.W, padx=4, pady=4)
        self.var_ins_layer = tk.StringVar()
        self.cmb_ins_layer = ttk.Combobox(form, textvariable=self.var_ins_layer, values=["[원본 유지 (Original)]", "0"])
        self.cmb_ins_layer.grid(row=1, column=3, sticky=tk.EW, padx=4, pady=4)

        ttk.Label(form, text="삽입 색상(Color):").grid(row=2, column=0, sticky=tk.W, padx=4, pady=4)
        self.var_ins_color = tk.StringVar()
        self.cmb_ins_color = ttk.Combobox(form, textvariable=self.var_ins_color, values=color_choices)
        self.cmb_ins_color.grid(row=2, column=1, sticky=tk.EW, padx=4, pady=4)

        ttk.Label(form, text="삽입 선타입(Linetype):").grid(row=2, column=2, sticky=tk.W, padx=4, pady=4)
        self.var_ins_ltype = tk.StringVar()
        self.cmb_ins_ltype = ttk.Combobox(form, textvariable=self.var_ins_ltype, values=ltype_choices)
        self.cmb_ins_ltype.grid(row=2, column=3, sticky=tk.EW, padx=4, pady=4)

        # --- 섹션 B: 블록 내부 객체(Entities) 일괄 변환 설정 ---
        ttk.Label(form, text="■ [2] 블록 내부 객체 일괄 조정 (선, 원, 폴리선, 문자 등 내부 요소)", font=("맑은 고딕", 9, "bold"), foreground="#333333").grid(
            row=3, column=0, columnspan=4, sticky=tk.W, pady=(10, 6)
        )

        ttk.Label(form, text="내부 객체 레이어:").grid(row=4, column=0, sticky=tk.W, padx=4, pady=4)
        self.var_ent_layer = tk.StringVar()
        self.cmb_ent_layer = ttk.Combobox(form, textvariable=self.var_ent_layer, values=["[원본 유지 (Original)]", "0"])
        self.cmb_ent_layer.grid(row=4, column=1, sticky=tk.EW, padx=4, pady=4)

        ttk.Label(form, text="내부 객체 색상:").grid(row=4, column=2, sticky=tk.W, padx=4, pady=4)
        self.var_ent_color = tk.StringVar()
        self.cmb_ent_color = ttk.Combobox(form, textvariable=self.var_ent_color, values=color_choices)
        self.cmb_ent_color.grid(row=4, column=3, sticky=tk.EW, padx=4, pady=4)

        ttk.Label(form, text="내부 객체 선타입:").grid(row=5, column=0, sticky=tk.W, padx=4, pady=4)
        self.var_ent_ltype = tk.StringVar()
        self.cmb_ent_ltype = ttk.Combobox(form, textvariable=self.var_ent_ltype, values=ltype_choices)
        self.cmb_ent_ltype.grid(row=5, column=1, sticky=tk.EW, padx=4, pady=4)

        ttk.Label(form, text="내부 텍스트 스타일:").grid(row=5, column=2, sticky=tk.W, padx=4, pady=4)
        self.var_ent_style = tk.StringVar()
        self.cmb_ent_style = ttk.Combobox(form, textvariable=self.var_ent_style, values=["[원본 유지 (Original)]", "Standard", "GHS", "돋움", "굴림", "맑은 고딕"])
        self.cmb_ent_style.grid(row=5, column=3, sticky=tk.EW, padx=4, pady=4)

        # --- 섹션 C: 새 도면에 텍스트 스타일 자동 생성 시 사용할 폰트 설정 ---
        ttk.Label(form, text="스타일 영문 폰트(SHX/TTF):").grid(row=6, column=0, sticky=tk.W, padx=4, pady=4)
        self.var_font_shx = tk.StringVar(value="romans.shx")
        self.cmb_font_shx = ttk.Combobox(form, textvariable=self.var_font_shx, values=["romans.shx", "simplex.shx", "txt.shx", "arial.ttf", "malgun.ttf", "gulim.ttc"])
        self.cmb_font_shx.grid(row=6, column=1, sticky=tk.EW, padx=4, pady=4)

        ttk.Label(form, text="스타일 한글 큰글꼴(BigFont):").grid(row=6, column=2, sticky=tk.W, padx=4, pady=4)
        self.var_bigfont_shx = tk.StringVar(value="whgtxt.shx")
        self.cmb_bigfont_shx = ttk.Combobox(form, textvariable=self.var_bigfont_shx, values=["whgtxt.shx", "ghs.shx", "ehdud.shx", "hgtxt.shx", ""])
        self.cmb_bigfont_shx.grid(row=6, column=3, sticky=tk.EW, padx=4, pady=4)

        # 하단 설정 저장 및 코드 미리보기 버튼
        action_bar = ttk.Frame(edit_group)
        action_bar.pack(fill=tk.X, pady=(10, 0))

        btn_save_setting = tk.Button(
            action_bar,
            text="💾 현재 블록의 속성 설정 변경사항 저장",
            bg="#005A9E", fg="white", font=("맑은 고딕", 9, "bold"),
            padx=14, pady=4, cursor="hand2",
            command=self.save_current_block_settings
        )
        btn_save_setting.pack(side=tk.LEFT)

        btn_view_code = ttk.Button(
            action_bar,
            text="📄 이 블록의 AutoLISP 생성 코드 미리보기 / 복사",
            command=self.preview_single_lisp_code
        )
        btn_view_code.pack(side=tk.RIGHT)

        # 상태 표시줄
        self.status_var = tk.StringVar(value="준비 완료. CAD에서 블록을 가져오거나 목록에서 블록을 선택하세요.")
        status_bar = ttk.Label(self.root, textvariable=self.status_var, relief=tk.SUNKEN, anchor=tk.W, padding=(8, 4))
        status_bar.pack(fill=tk.X, side=tk.BOTTOM)

    # --------------------------------------------------------------------------
    # Treeview 목록 관리 및 체크박스 이벤트
    # --------------------------------------------------------------------------
    def _refresh_tree(self):
        selected_id = self.current_block_id
        prev_sel = [i for i in self.tree.selection() if i in self.blocks_db]  # 다중 선택 유지용
        self.tree.delete(*self.tree.get_children())

        for bid, rec in self.blocks_db.items():
            settings = rec.get("settings", {})
            checked = rec.get("checked", True)
            chk_mark = "☑" if checked else "☐"

            orig_name = rec.get("main_block", "UNKNOWN")
            use_orig = settings.get("use_original_all", False)
            disp_name = orig_name if use_orig else settings.get("block_name", orig_name)

            # 메인 블록 내 엔티티 개수 계산
            ent_cnt = 0
            for b in rec.get("blocks", []):
                if b.get("name", "").upper() in (orig_name.upper(), rec.get("raw_block_name", "").upper()):
                    ent_cnt = len(b.get("entities", []))
                    break

            ins_lay = rec.get("insert_layer", "0") if use_orig else settings.get("insert_layer", "[원본 유지 (Original)]")
            if ins_lay.startswith("[원본"):
                ins_lay = f"{rec.get('insert_layer', '0')} (원본)"

            self.tree.insert("", tk.END, iid=bid, values=(chk_mark, disp_name, orig_name, ent_cnt, ins_lay))

        if selected_id and selected_id in self.blocks_db:
            self.tree.selection_set(prev_sel if prev_sel else [selected_id])
            self.tree.see(selected_id)
        elif prev_sel:
            self.tree.selection_set(prev_sel)
        elif self.blocks_db:
            first_id = next(iter(self.blocks_db))
            self.tree.selection_set(first_id)
            self._load_block_to_form(first_id)

    def _on_tree_click(self, event):
        region = self.tree.identify("region", event.x, event.y)
        if region == "cell":
            col = self.tree.identify_column(event.x)
            row_id = self.tree.identify_row(event.y)
            if row_id and col == "#1":  # 첫 번째 열(체크박스) 클릭 시 토글
                cur_val = self.blocks_db[row_id].get("checked", True)
                self.blocks_db[row_id]["checked"] = not cur_val
                self.save_database()
                vals = list(self.tree.item(row_id, "values"))
                vals[0] = "☑" if not cur_val else "☐"
                self.tree.item(row_id, values=vals)

    def _on_tree_select(self, event):
        sel = self.tree.selection()
        if sel:
            # Ctrl/Shift 다중 선택 시 마지막에 클릭한(포커스) 항목을 우측 폼에 표시
            focus_id = self.tree.focus()
            target = focus_id if focus_id in sel else sel[-1]
            self._load_block_to_form(target)

    def _set_all_checks(self, state: bool):
        for bid in self.blocks_db:
            self.blocks_db[bid]["checked"] = state
        self.save_database()
        self._refresh_tree()

    def _set_selected_checks(self, state: bool):
        """Ctrl/Shift 로 다중 선택한 항목들의 체크(☑) 상태만 일괄 변경"""
        sel = [bid for bid in self.tree.selection() if bid in self.blocks_db]
        if not sel:
            self.status_var.set("먼저 목록에서 항목을 선택하세요. (Ctrl+클릭 / Shift+클릭으로 다중 선택)")
            return
        for bid in sel:
            self.blocks_db[bid]["checked"] = state
        self.save_database()
        self._refresh_tree()
        state_txt = "체크(☑)" if state else "해제(☐)"
        self.status_var.set(f"선택한 {len(sel)}개 항목을 {state_txt}했습니다.")

    # --------------------------------------------------------------------------
    # 선택한 블록 데이터를 우측 편집 폼 및 미리보기 캔버스에 로드
    # --------------------------------------------------------------------------
    def _load_block_to_form(self, block_id: str):
        self.current_block_id = block_id
        rec = self.blocks_db.get(block_id)
        if not rec:
            return

        orig_name = rec.get("main_block", "")
        settings = rec.get("settings", {})

        # 레이어 및 스타일 콤보박스 후보 목록에 원본 레이어/스타일 추가
        orig_layers = [lay["name"] for lay in rec.get("layers", [])]
        layer_opts = ["[원본 유지 (Original)]", "0"] + [l for l in orig_layers if l != "0"]
        self.cmb_ins_layer["values"] = layer_opts
        self.cmb_ent_layer["values"] = layer_opts

        orig_styles = [sty["name"] for sty in rec.get("styles", [])]
        style_opts = ["[원본 유지 (Original)]", "Standard", "GHS", "돋움", "굴림", "맑은 고딕"]
        for s in orig_styles:
            if s not in style_opts:
                style_opts.append(s)
        self.cmb_ent_style["values"] = style_opts

        # 설정값 반영
        self.var_use_original_all.set(settings.get("use_original_all", False))
        self.var_blk_name.set(settings.get("block_name", orig_name))
        self.var_ins_layer.set(settings.get("insert_layer", "[원본 유지 (Original)]"))
        self.var_ins_color.set(settings.get("insert_color", "[원본 유지 (Original)]"))
        self.var_ins_ltype.set(settings.get("insert_linetype", "[원본 유지 (Original)]"))

        self.var_ent_layer.set(settings.get("entity_layer", "[원본 유지 (Original)]"))
        self.var_ent_color.set(settings.get("entity_color", "[원본 유지 (Original)]"))
        self.var_ent_ltype.set(settings.get("entity_linetype", "[원본 유지 (Original)]"))
        self.var_ent_style.set(settings.get("entity_textstyle", "[원본 유지 (Original)]"))
        self.var_font_shx.set(settings.get("textstyle_font", "romans.shx"))
        self.var_bigfont_shx.set(settings.get("textstyle_bigfont", "whgtxt.shx"))

        self._on_toggle_use_original()

        # 요약 텍스트 구성 (중첩 블록 내부 객체 수까지 합산 표시)
        type_counts = {}
        nested_ent_total = 0
        for b in rec.get("blocks", []):
            if b.get("name", "").upper() in (orig_name.upper(), rec.get("raw_block_name", "").upper()):
                for ent in b.get("entities", []):
                    et = ent.get("etype", "ETC")
                    type_counts[et] = type_counts.get(et, 0) + 1
            else:
                nested_ent_total += len(b.get("entities", []))

        summary_parts = [f"{k}: {v}개" for k, v in type_counts.items()]
        nested_cnt = max(0, len(rec.get("blocks", [])) - 1)
        nested_str = f" (중첩 블록 {nested_cnt}종 / 하위 객체 {nested_ent_total}개 포함)" if nested_cnt > 0 else ""
        self.lbl_summary.config(
            text=f"원본 블록명: {orig_name}{nested_str}  |  원본 삽입 레이어: {rec.get('insert_layer', '0')}  |  내부 구성: {', '.join(summary_parts)}"
        )

        self._draw_preview()

    def _on_toggle_use_original(self):
        use_orig = self.var_use_original_all.get()
        state = tk.DISABLED if use_orig else tk.NORMAL
        for w in (
            self.ent_blk_name, self.cmb_ins_layer, self.cmb_ins_color, self.cmb_ins_ltype,
            self.cmb_ent_layer, self.cmb_ent_color, self.cmb_ent_ltype, self.cmb_ent_style,
            self.cmb_font_shx, self.cmb_bigfont_shx
        ):
            w.configure(state=state)
        self._draw_preview()

    def _reset_settings_to_original(self):
        if not self.current_block_id:
            return
        rec = self.blocks_db[self.current_block_id]
        self.var_use_original_all.set(False)
        self.var_blk_name.set(rec.get("main_block", ""))
        self.var_ins_layer.set("[원본 유지 (Original)]")
        self.var_ins_color.set("[원본 유지 (Original)]")
        self.var_ins_ltype.set("[원본 유지 (Original)]")
        self.var_ent_layer.set("[원본 유지 (Original)]")
        self.var_ent_color.set("[원본 유지 (Original)]")
        self.var_ent_ltype.set("[원본 유지 (Original)]")
        self.var_ent_style.set("[원본 유지 (Original)]")
        self._on_toggle_use_original()
        self.save_current_block_settings(silent=True)
        self.status_var.set("모든 속성 설정이 [원본 유지 (Original)] 상태로 초기화되었습니다.")

    def save_current_block_settings(self, silent: bool = False):
        if not self.current_block_id or self.current_block_id not in self.blocks_db:
            return
        rec = self.blocks_db[self.current_block_id]
        new_name = self.var_blk_name.get().strip() or rec.get("main_block", "BLOCK")

        rec["settings"] = {
            "use_original_all": bool(self.var_use_original_all.get()),
            "block_name": new_name,
            "insert_layer": self.var_ins_layer.get().strip(),
            "insert_color": self.var_ins_color.get().strip(),
            "insert_linetype": self.var_ins_ltype.get().strip(),
            "entity_layer": self.var_ent_layer.get().strip(),
            "entity_color": self.var_ent_color.get().strip(),
            "entity_linetype": self.var_ent_ltype.get().strip(),
            "entity_textstyle": self.var_ent_style.get().strip(),
            "textstyle_font": self.var_font_shx.get().strip(),
            "textstyle_bigfont": self.var_bigfont_shx.get().strip(),
        }
        self.save_database()
        self._refresh_tree()
        self._draw_preview()
        if not silent:
            self.status_var.set(f"'{new_name}' 블록의 속성 설정이 성공적으로 저장되었습니다.")

    # --------------------------------------------------------------------------
    # 2D 블록 형상 미리보기 렌더링 엔진 (ELLIPSE, HATCH, Bulge 호, ACI 256 색상 완벽 지원)
    # --------------------------------------------------------------------------
    @staticmethod
    def _bulge_to_arc_pts(p1, p2, bulge: float, steps: int = 12):
        """폴리선이나 해치 경계의 Bulge 값을 다각형 호 점들로 변환합니다."""
        if abs(bulge) < 1e-6:
            return [p1, p2]
        x1, y1 = float(p1[0]), float(p1[1])
        x2, y2 = float(p2[0]), float(p2[1])
        dx, dy = x2 - x1, y2 - y1
        chord = math.hypot(dx, dy)
        if chord < 1e-9:
            return [p1]
        theta = 4.0 * math.atan(bulge)
        radius = chord / (2.0 * math.sin(abs(theta) / 2.0))
        mid_x, mid_y = (x1 + x2) / 2.0, (y1 + y2) / 2.0
        dist_to_center = radius * math.cos(theta / 2.0)
        # 법선 방향 계산
        nx, ny = -dy / chord, dx / chord
        if bulge < 0:
            nx, ny = -nx, -ny
        cx = mid_x + nx * dist_to_center
        cy = mid_y + ny * dist_to_center
        start_ang = math.atan2(y1 - cy, x1 - cx)
        pts = []
        for i in range(steps + 1):
            ang = start_ang + theta * (i / steps)
            pts.append((cx + abs(radius) * math.cos(ang), cy + abs(radius) * math.sin(ang)))
        return pts

    @staticmethod
    def _parse_hatch_loops(dxf_list):
        """
        HATCH 엔티티의 DXF 그룹 리스트에서 경계 루프(폴리선형 및 엣지형: 선/호/타원)의
        외곽 다각형 좌표 리스트들을 정밀하게 복원합니다.
        """
        loops = []
        n = len(dxf_list)
        i = 0
        num_loops = 0
        while i < n:
            if dxf_list[i][0] == 91:
                num_loops = int(dxf_list[i][2])
                i += 1
                break
            i += 1

        for _ in range(num_loops):
            while i < n and dxf_list[i][0] != 92:
                i += 1
            if i >= n:
                break
            loop_flag = int(dxf_list[i][2])
            i += 1

            # 1) 폴리선형 경계 (bit 2 설정)
            if loop_flag & 2:
                has_bulge = 0
                is_closed = 1
                num_verts = 0
                while i < n and dxf_list[i][0] in (72, 73, 93):
                    c, _, v = dxf_list[i]
                    if c == 72:
                        has_bulge = int(v)
                    elif c == 73:
                        is_closed = int(v)
                    elif c == 93:
                        num_verts = int(v)
                        i += 1
                        break
                    i += 1
                verts = []
                for _v in range(num_verts):
                    if i < n and dxf_list[i][0] == 10:
                        pt = dxf_list[i][2]
                        i += 1
                        b_val = 0.0
                        if has_bulge and i < n and dxf_list[i][0] == 42:
                            b_val = float(dxf_list[i][2])
                            i += 1
                        verts.append((pt, b_val))
                if verts:
                    poly_pts = []
                    v_cnt = len(verts)
                    for vi in range(v_cnt):
                        p_cur, b_cur = verts[vi]
                        if vi < v_cnt - 1:
                            p_nxt = verts[vi + 1][0]
                            seg = BlockManagerApp._bulge_to_arc_pts(p_cur, p_nxt, b_cur)
                            poly_pts.extend(seg[:-1])
                        elif is_closed and v_cnt > 1:
                            p_nxt = verts[0][0]
                            seg = BlockManagerApp._bulge_to_arc_pts(p_cur, p_nxt, b_cur)
                            poly_pts.extend(seg)
                        else:
                            poly_pts.append(p_cur)
                    if len(poly_pts) >= 2:
                        loops.append(poly_pts)
            else:
                # 2) 일반 엣지(Edge) 경계 (선=1, 원호=2, 타원호=3, 스플라인=4)
                while i < n and dxf_list[i][0] != 93:
                    if dxf_list[i][0] in (92, 75, 76):
                        break
                    i += 1
                if i >= n or dxf_list[i][0] != 93:
                    continue
                num_edges = int(dxf_list[i][2])
                i += 1
                loop_pts = []
                for _e in range(num_edges):
                    while i < n and dxf_list[i][0] != 72:
                        if dxf_list[i][0] in (92, 97, 75):
                            break
                        i += 1
                    if i >= n or dxf_list[i][0] != 72:
                        break
                    edge_type = int(dxf_list[i][2])
                    i += 1
                    if edge_type == 1:  # 직선 엣지
                        p1, p2 = None, None
                        while i < n and dxf_list[i][0] in (10, 11):
                            if dxf_list[i][0] == 10:
                                p1 = dxf_list[i][2]
                            elif dxf_list[i][0] == 11:
                                p2 = dxf_list[i][2]
                            i += 1
                            if p1 is not None and p2 is not None:
                                break
                        if p1 and p2:
                            loop_pts.extend([p1, p2])
                    elif edge_type == 2:  # 원호(Arc) 엣지
                        center, rad, a1, a2, ccw = None, 0.0, 0.0, 360.0, 1
                        while i < n and dxf_list[i][0] in (10, 40, 50, 51, 73):
                            c, _, v = dxf_list[i]
                            if c == 10:
                                center = v
                            elif c == 40:
                                rad = float(v)
                            elif c == 50:
                                a1 = float(v)
                            elif c == 51:
                                a2 = float(v)
                            elif c == 73:
                                ccw = int(v)
                                i += 1
                                break
                            i += 1
                        if center and rad > 0:
                            # 해치 엣지의 각도는 도(Degree) 또는 라디안일 수 있으므로 판별
                            if abs(a1) > 2 * math.pi + 0.1 or abs(a2) > 2 * math.pi + 0.1:
                                r1, r2 = math.radians(a1), math.radians(a2)
                            else:
                                r1, r2 = a1, a2
                            if not ccw:
                                r1, r2 = -r1, -r2
                            if r2 <= r1:
                                r2 += 2.0 * math.pi
                            steps = 20
                            for s in range(steps + 1):
                                ang = r1 + (r2 - r1) * (s / steps)
                                loop_pts.append((center[0] + rad * math.cos(ang), center[1] + rad * math.sin(ang)))
                    elif edge_type == 3:  # 타원호(Ellipse) 엣지
                        center, maj, ratio, a1, a2, ccw = None, None, 1.0, 0.0, 2 * math.pi, 1
                        while i < n and dxf_list[i][0] in (10, 11, 40, 50, 51, 73):
                            c, _, v = dxf_list[i]
                            if c == 10:
                                center = v
                            elif c == 11:
                                maj = v
                            elif c == 40:
                                ratio = float(v)
                            elif c == 50:
                                a1 = float(v)
                            elif c == 51:
                                a2 = float(v)
                            elif c == 73:
                                ccw = int(v)
                                i += 1
                                break
                            i += 1
                        if center and maj:
                            if abs(a1) > 2 * math.pi + 0.1 or abs(a2) > 2 * math.pi + 0.1:
                                r1, r2 = math.radians(a1), math.radians(a2)
                            else:
                                r1, r2 = a1, a2
                            if r2 <= r1:
                                r2 += 2.0 * math.pi
                            ux, uy = float(maj[0]), float(maj[1])
                            vx, vy = -uy * ratio, ux * ratio
                            steps = 24
                            for s in range(steps + 1):
                                t = r1 + (r2 - r1) * (s / steps)
                                px = center[0] + ux * math.cos(t) + vx * math.sin(t)
                                py = center[1] + uy * math.cos(t) + vy * math.sin(t)
                                loop_pts.append((px, py))
                    else:
                        i += 1
                if len(loop_pts) >= 2:
                    loops.append(loop_pts)
        return loops

    def _draw_preview(self):
        self.canvas.delete("all")
        if not self.current_block_id or self.current_block_id not in self.blocks_db:
            return

        rec = self.blocks_db[self.current_block_id]
        orig_name = rec.get("main_block", "")
        raw_name = rec.get("raw_block_name", orig_name)

        # 블록 이름으로 빠르게 블록 정의를 찾을 수 있도록 사전(Map) 구축
        blocks_map = {}
        main_blk = None
        for b in rec.get("blocks", []):
            bn_upper = b.get("name", "").upper()
            blocks_map[bn_upper] = b
            if bn_upper in (orig_name.upper(), raw_name.upper()):
                main_blk = b

        if not main_blk and rec.get("blocks"):
            main_blk = rec["blocks"][-1]
        if not main_blk:
            return

        bpt = main_blk.get("base_point", [0.0, 0.0, 0.0])
        bx, by = float(bpt[0]), float(bpt[1])

        # 레이어별 색상 사전 구성 (ByLayer 색상 추적용)
        layer_color_map = {"0": 7}
        for lay in rec.get("layers", []):
            layer_color_map[lay.get("name", "0").upper()] = int(lay.get("color", 7))

        # 현재 메인 블록 삽입 색상 및 레이어 확인 (ByBlock 색상 추적용)
        use_orig = self.var_use_original_all.get()
        orig_ins_col = int(rec.get("insert_color", 256))
        orig_ins_lay = rec.get("insert_layer", "0")
        if orig_ins_col == 256:
            orig_ins_col = layer_color_map.get(orig_ins_lay.upper(), 7)

        ins_col_str = "[원본 유지 (Original)]" if use_orig else self.var_ins_color.get()
        root_block_aci = LispCodeGenerator.resolve_color_value(ins_col_str, orig_ins_col)
        if root_block_aci == 256:
            ins_lay_str = orig_ins_lay if use_orig else self.var_ins_layer.get()
            if ins_lay_str.startswith("[원본"):
                ins_lay_str = orig_ins_lay
            root_block_aci = layer_color_map.get(ins_lay_str.upper(), 7)

        # 내부 객체 색상/레이어 오버라이드 확인
        ent_color_str = "[원본 유지 (Original)]" if use_orig else self.var_ent_color.get()
        ent_layer_str = "[원본 유지 (Original)]" if use_orig else self.var_ent_layer.get()

        override_col_aci = None
        if ent_color_str and not ent_color_str.startswith("[원본"):
            override_col_aci = LispCodeGenerator.resolve_color_value(ent_color_str, 256)

        def resolve_entity_aci(dxf_list, parent_byblock_aci: int, parent_layer: str) -> int:
            """
            개별 엔티티(또는 중첩 INSERT)의 실제 ACI 색상 번호를 계산합니다.
            - 0번 레이어에 있는 중첩 블록 내부 객체는 상위 INSERT의 레이어를 따라갑니다.
            - ByBlock(0) 색상은 상위 블록(INSERT)의 색상을 따라갑니다.
            """
            ent_lay = next((str(v) for c, _, v in dxf_list if c == 8), "0")
            if ent_layer_str and not ent_layer_str.startswith("[원본"):
                ent_lay = ent_layer_str
            elif ent_lay == "0" and parent_layer:
                ent_lay = parent_layer

            c_aci = next((int(v) for c, _, v in dxf_list if c == 62), 256)
            if override_col_aci is not None:
                c_aci = override_col_aci

            if c_aci == 256:  # ByLayer
                c_aci = layer_color_map.get(ent_lay.upper(), 7)
            elif c_aci == 0:  # ByBlock
                c_aci = parent_byblock_aci
            return c_aci

        shapes = []  # (type, points_list, hex_color, extra)
        xs, ys = [bx], [by]

        def collect_block_shapes(blk_dict, tf_fn, parent_aci: int, parent_layer: str, depth: int = 0):
            """
            블록 내부의 모든 엔티티를 순회하며 도형 좌표를 수집합니다.
            내부에 중첩 블록(INSERT)이 있으면 위치/축척/회전/기준점 변환 함수를 합성하여 재귀적으로 렌더링합니다.
            """
            if depth > 12 or not blk_dict:
                return

            for ent in blk_dict.get("entities", []):
                etype = ent.get("etype", "")
                dxf = ent.get("dxf", [])
                ent_aci = resolve_entity_aci(dxf, parent_aci, parent_layer)
                hex_col = aci_to_hex_color(ent_aci)

                # [중첩 블록(INSERT) 재귀 처리]
                if etype == "INSERT":
                    sub_bname = next((str(v) for c, _, v in dxf if c == 2), "")
                    sub_blk = blocks_map.get(sub_bname.upper())
                    if not sub_blk:
                        continue

                    ins_pt = next((v for c, _, v in dxf if c == 10), [0.0, 0.0, 0.0])
                    sx = float(next((v for c, _, v in dxf if c == 41), 1.0))
                    sy = float(next((v for c, _, v in dxf if c == 42), 1.0))
                    rot_rad = float(next((v for c, _, v in dxf if c == 50), 0.0))

                    # 다중 삽입(MINSERT) 행/열 반복 지원
                    col_cnt = max(1, int(next((v for c, _, v in dxf if c == 70), 1)))
                    row_cnt = max(1, int(next((v for c, _, v in dxf if c == 71), 1)))
                    col_sp = float(next((v for c, _, v in dxf if c == 44), 0.0))
                    row_sp = float(next((v for c, _, v in dxf if c == 45), 0.0))

                    sub_bpt = sub_blk.get("base_point", [0.0, 0.0, 0.0])
                    sbx, sby = float(sub_bpt[0]), float(sub_bpt[1])
                    ix, iy = float(ins_pt[0]), float(ins_pt[1])

                    cos_r = math.cos(rot_rad)
                    sin_r = math.sin(rot_rad)

                    sub_ent_lay = next((str(v) for c, _, v in dxf if c == 8), "0")
                    eff_layer = parent_layer if sub_ent_lay == "0" else sub_ent_lay

                    for r_idx in range(row_cnt):
                        for c_idx in range(col_cnt):
                            off_x = c_idx * col_sp
                            off_y = r_idx * row_sp

                            def make_child_tf(p_tf, _ix=ix, _iy=iy, _sx=sx, _sy=sy,
                                              _cos=cos_r, _sin=sin_r, _sbx=sbx, _sby=sby,
                                              _ox=off_x, _oy=off_y):
                                def _child_tf(x, y):
                                    # 1. 하위 블록 자체 기준점(Base point) 차감 및 축척 적용
                                    dx = (float(x) - _sbx) * _sx + _ox
                                    dy = (float(y) - _sby) * _sy + _oy
                                    # 2. 회전 및 삽입점 이동
                                    rx = _ix + (dx * _cos - dy * _sin)
                                    ry = _iy + (dx * _sin + dy * _cos)
                                    # 3. 상위 변환 함수 적용
                                    return p_tf(rx, ry)
                                return _child_tf

                            child_tf = make_child_tf(tf_fn)
                            collect_block_shapes(sub_blk, child_tf, ent_aci, eff_layer, depth + 1)
                    continue

                # [일반 도형 엔티티 처리]
                if etype == "LINE":
                    p1 = next((v for c, _, v in dxf if c == 10), None)
                    p2 = next((v for c, _, v in dxf if c == 11), None)
                    if p1 and p2:
                        tp1 = tf_fn(p1[0], p1[1])
                        tp2 = tf_fn(p2[0], p2[1])
                        xs.extend([tp1[0], tp2[0]])
                        ys.extend([tp1[1], tp2[1]])
                        shapes.append(("LINE", [tp1, tp2], hex_col, None))

                elif etype == "LWPOLYLINE":
                    verts = []
                    cur_pt = None
                    cur_bulge = 0.0
                    for c, _, v in dxf:
                        if c == 10:
                            if cur_pt is not None:
                                verts.append((cur_pt, cur_bulge))
                            cur_pt = v
                            cur_bulge = 0.0
                        elif c == 42 and cur_pt is not None:
                            cur_bulge = float(v)
                    if cur_pt is not None:
                        verts.append((cur_pt, cur_bulge))

                    flag = next((v for c, _, v in dxf if c == 70), 0)
                    is_closed = bool(int(flag) & 1)
                    if verts:
                        poly_pts = []
                        v_cnt = len(verts)
                        for vi in range(v_cnt):
                            p_cur, b_cur = verts[vi]
                            if vi < v_cnt - 1:
                                seg = self._bulge_to_arc_pts(p_cur, verts[vi + 1][0], b_cur)
                                poly_pts.extend(seg[:-1])
                            elif is_closed and v_cnt > 1:
                                seg = self._bulge_to_arc_pts(p_cur, verts[0][0], b_cur)
                                poly_pts.extend(seg)
                            else:
                                poly_pts.append(p_cur)
                        tf_pts = [tf_fn(p[0], p[1]) for p in poly_pts]
                        for p in tf_pts:
                            xs.append(p[0])
                            ys.append(p[1])
                        shapes.append(("POLY", tf_pts, hex_col, None))

                elif etype == "POLYLINE":
                    pts = []
                    for sub_dxf in ent.get("sub_entities", []):
                        p = next((v for c, _, v in sub_dxf if c == 10), None)
                        if p:
                            pts.append(tf_fn(p[0], p[1]))
                    flag = next((v for c, _, v in dxf if c == 70), 0)
                    if pts:
                        for p in pts:
                            xs.append(p[0])
                            ys.append(p[1])
                        if (int(flag) & 1) and len(pts) > 2:
                            pts = pts + [pts[0]]
                        shapes.append(("POLY", pts, hex_col, None))

                elif etype == "CIRCLE":
                    center = next((v for c, _, v in dxf if c == 10), None)
                    rad = next((v for c, _, v in dxf if c == 40), None)
                    if center and rad:
                        cx, cy = float(center[0]), float(center[1])
                        r = float(rad)
                        # 중첩 블록의 비균일 축척/회전에도 정확히 대응하도록 다각형 점들로 변환
                        steps = 36
                        c_pts = []
                        for i in range(steps + 1):
                            ang = (2.0 * math.pi) * (i / steps)
                            tp = tf_fn(cx + r * math.cos(ang), cy + r * math.sin(ang))
                            c_pts.append(tp)
                            xs.append(tp[0])
                            ys.append(tp[1])
                        shapes.append(("POLY", c_pts, hex_col, None))

                elif etype == "ARC":
                    center = next((v for c, _, v in dxf if c == 10), None)
                    rad = next((v for c, _, v in dxf if c == 40), None)
                    ang1 = next((v for c, _, v in dxf if c == 50), 0.0)
                    ang2 = next((v for c, _, v in dxf if c == 51), math.pi * 2)
                    if center and rad:
                        cx, cy = float(center[0]), float(center[1])
                        r = float(rad)
                        a1, a2 = float(ang1), float(ang2)
                        if a2 < a1:
                            a2 += 2.0 * math.pi
                        steps = 28
                        arc_pts = []
                        for i in range(steps + 1):
                            ang = a1 + (a2 - a1) * (i / steps)
                            tp = tf_fn(cx + r * math.cos(ang), cy + r * math.sin(ang))
                            arc_pts.append(tp)
                            xs.append(tp[0])
                            ys.append(tp[1])
                        shapes.append(("POLY", arc_pts, hex_col, None))

                elif etype == "ELLIPSE":
                    center = next((v for c, _, v in dxf if c == 10), None)
                    maj_vec = next((v for c, _, v in dxf if c == 11), None)
                    ratio = float(next((v for c, _, v in dxf if c == 40), 1.0))
                    t1 = float(next((v for c, _, v in dxf if c == 41), 0.0))
                    t2 = float(next((v for c, _, v in dxf if c == 42), math.pi * 2.0))
                    if center and maj_vec:
                        cx, cy = float(center[0]), float(center[1])
                        ux, uy = float(maj_vec[0]), float(maj_vec[1])
                        vx, vy = -uy * ratio, ux * ratio
                        if t2 <= t1:
                            t2 += math.pi * 2.0
                        steps = 48
                        ell_pts = []
                        for i in range(steps + 1):
                            t = t1 + (t2 - t1) * (i / steps)
                            px = cx + ux * math.cos(t) + vx * math.sin(t)
                            py = cy + uy * math.cos(t) + vy * math.sin(t)
                            tp = tf_fn(px, py)
                            ell_pts.append(tp)
                            xs.append(tp[0])
                            ys.append(tp[1])
                        shapes.append(("POLY", ell_pts, hex_col, None))

                elif etype == "HATCH":
                    pat_name = str(next((v for c, _, v in dxf if c == 2), "SOLID")).upper()
                    solid_flag = int(next((v for c, _, v in dxf if c == 70), 1))
                    is_solid = (solid_flag == 1 or pat_name == "SOLID")
                    loops = self._parse_hatch_loops(dxf)
                    for lpts in loops:
                        tf_lpts = [tf_fn(p[0], p[1]) for p in lpts]
                        for p in tf_lpts:
                            xs.append(p[0])
                            ys.append(p[1])
                        if is_solid and len(tf_lpts) >= 3:
                            shapes.append(("HATCH_SOLID", tf_lpts, hex_col, None))
                        else:
                            shapes.append(("POLY", tf_lpts, hex_col, None))

                elif etype == "SPLINE":
                    fit_pts = [v for c, _, v in dxf if c == 11]
                    ctrl_pts = [v for c, _, v in dxf if c == 10]
                    s_pts = fit_pts if len(fit_pts) >= 2 else ctrl_pts
                    if len(s_pts) >= 2:
                        tf_spts = [tf_fn(p[0], p[1]) for p in s_pts]
                        for p in tf_spts:
                            xs.append(p[0])
                            ys.append(p[1])
                        shapes.append(("POLY", tf_spts, hex_col, None))

                elif etype in ("SOLID", "3DFACE"):
                    p10 = next((v for c, _, v in dxf if c == 10), None)
                    p11 = next((v for c, _, v in dxf if c == 11), None)
                    p12 = next((v for c, _, v in dxf if c == 12), None)
                    p13 = next((v for c, _, v in dxf if c == 13), p12)
                    pts = [p for p in (p10, p11, p13, p12) if p is not None]
                    if len(pts) >= 3:
                        tf_pts = [tf_fn(p[0], p[1]) for p in pts]
                        for p in tf_pts:
                            xs.append(p[0])
                            ys.append(p[1])
                        shapes.append(("HATCH_SOLID", tf_pts, hex_col, None))

                elif etype in ("TEXT", "MTEXT", "ATTDEF"):
                    pt = next((v for c, _, v in dxf if c == 10), None)
                    txt = next((v for c, _, v in dxf if c in (1, 2)), "TEXT")
                    if pt:
                        tp = tf_fn(pt[0], pt[1])
                        xs.append(tp[0])
                        ys.append(tp[1])
                        shapes.append(("TEXT", [tp], hex_col, str(txt)))

        # 최상위 메인 블록부터 탐색 시작 (기본 항등 변환)
        collect_block_shapes(
            main_blk,
            tf_fn=lambda x, y: (float(x), float(y)),
            parent_aci=root_block_aci,
            parent_layer=orig_ins_lay,
            depth=0
        )

        # 캔버스 크기 확인
        cw = max(self.canvas.winfo_width(), 200)
        ch = max(self.canvas.winfo_height(), 150)
        pad = 30

        min_x, max_x = min(xs), max(xs)
        min_y, max_y = min(ys), max(ys)
        span_x = max(max_x - min_x, 1e-4)
        span_y = max(max_y - min_y, 1e-4)

        scale = min((cw - pad * 2) / span_x, (ch - pad * 2) / span_y)
        cx_mid = (min_x + max_x) / 2.0
        cy_mid = (min_y + max_y) / 2.0

        def to_screen(wx, wy):
            sx = (cw / 2.0) + (wx - cx_mid) * scale
            sy = (ch / 2.0) - (wy - cy_mid) * scale
            return sx, sy

        # 1) 솔리드 해치(면 채우기)를 먼저 바닥에 렌더링하고, 선/원/타원을 그 위에 렌더링
        solid_shapes = [s for s in shapes if s[0] == "HATCH_SOLID"]
        line_shapes = [s for s in shapes if s[0] != "HATCH_SOLID"]

        for _, pts, col, _ in solid_shapes:
            if len(pts) >= 3:
                flat_coords = []
                for p in pts:
                    sx, sy = to_screen(p[0], p[1])
                    flat_coords.extend([sx, sy])
                self.canvas.create_polygon(*flat_coords, fill=col, outline=col, width=1)

        for stype, pts, col, extra in line_shapes:
            if stype == "LINE":
                x1, y1 = to_screen(pts[0][0], pts[0][1])
                x2, y2 = to_screen(pts[1][0], pts[1][1])
                self.canvas.create_line(x1, y1, x2, y2, fill=col, width=1.8)
            elif stype == "POLY" and len(pts) >= 2:
                flat_coords = []
                for p in pts:
                    sx, sy = to_screen(p[0], p[1])
                    flat_coords.extend([sx, sy])
                self.canvas.create_line(*flat_coords, fill=col, width=1.8)
            elif stype == "CIRCLE":
                sx, sy = to_screen(pts[0][0], pts[0][1])
                sr = extra * scale
                self.canvas.create_oval(sx - sr, sy - sr, sx + sr, sy + sr, outline=col, width=1.8)
            elif stype == "TEXT":
                sx, sy = to_screen(pts[0][0], pts[0][1])
                disp_txt = extra[:15] if extra else "TXT"
                self.canvas.create_text(sx, sy, text=disp_txt, fill=col, font=("맑은 고딕", 9, "bold"), anchor=tk.SW)

        # 2) 블록 기준점(Base Point) 노란색 미니 마커 표시
        bsx, bsy = to_screen(bx, by)
        self.canvas.create_line(bsx - 7, bsy, bsx + 7, bsy, fill="#FFD60A", width=1.2)
        self.canvas.create_line(bsx, bsy - 7, bsx, bsy + 7, fill="#FFD60A", width=1.2)
        self.canvas.create_oval(bsx - 2.5, bsy - 2.5, bsx + 2.5, bsy + 2.5, outline="#FFD60A", width=1.2)

    # --------------------------------------------------------------------------
    # AutoCAD COM 연결 헬퍼
    # --------------------------------------------------------------------------
    def _get_acad_doc(self):
        if not HAS_WIN32COM:
            messagebox.showerror(
                "모듈 누락",
                "pywin32 모듈이 설치되어 있지 않습니다.\nCMD 창에서 'pip install pywin32'를 실행해 주세요."
            )
            return None, None
        try:
            pythoncom.CoInitialize()
            acad = win32com.client.GetActiveObject("AutoCAD.Application")
            doc = acad.ActiveDocument
            return acad, doc
        except Exception:
            messagebox.showwarning(
                "AutoCAD 연결 실패",
                "현재 실행 중인 AutoCAD를 찾을 수 없거나 명령 대기 상태가 아닙니다.\n"
                "AutoCAD 도면을 열어두고 ESC를 눌러 명령줄을 비운 뒤 다시 시도해 주세요."
            )
            return None, None

    # --------------------------------------------------------------------------
    # 기능 1: CAD에서 블록 선택하여 가져오기 (추출)
    # --------------------------------------------------------------------------
    def extract_block_from_cad(self):
        acad, doc = self._get_acad_doc()
        if not doc:
            return

        tmp_dir = self.base_dir
        json_out_path = os.path.join(tmp_dir, "tmp_cad_block_extract.json")
        lsp_tmp_path = os.path.join(tmp_dir, "tmp_cad_block_extractor.lsp")

        if os.path.exists(json_out_path):
            try:
                os.remove(json_out_path)
            except OSError:
                pass

        # LISP 내 경로 슬래시 변환 및 CP949 안전 저장
        lisp_path_for_cad = json_out_path.replace("\\", "/")
        lisp_code = LISP_EXTRACTOR_TEMPLATE.replace("__OUTPATH__", lisp_path_for_cad)
        lisp_code = sanitize_for_cp949(lisp_code)

        with open(lsp_tmp_path, "w", encoding="cp949", errors="replace") as f:
            f.write(lisp_code)

        self.status_var.set("AutoCAD 창으로 이동하여 저장할 블록(INSERT) 객체를 클릭하세요...")
        self.root.update()

        try:
            acad.Visible = True
        except Exception:
            pass

        # AutoCAD에서 임시 추출 LISP 로드 및 실행
        load_cmd = f'(load "{lsp_tmp_path.replace(chr(92), "/")}")\n'
        doc.SendCommand(load_cmd)

        # 파일이 생성될 때까지 폴링 대기 (최대 120초)
        start_time = time.time()
        while time.time() - start_time < 120:
            self.root.update()
            if os.path.exists(json_out_path) and os.path.getsize(json_out_path) > 10:
                time.sleep(0.25)  # 파일 쓰기 완료 보장
                break
            time.sleep(0.15)

        if not os.path.exists(json_out_path):
            self.status_var.set("블록 추출 시간 초과 또는 취소되었습니다.")
            return

        # 추출된 JSON 파싱 (AutoCAD가 CP949로 출력하므로 cp949 우선, utf-8 폴백)
        raw_text = ""
        for enc in ("cp949", "utf-8"):
            try:
                with open(json_out_path, "r", encoding=enc) as f:
                    raw_text = f.read()
                break
            except Exception:
                continue

        try:
            extracted = json.loads(raw_text)
        except Exception as e:
            messagebox.showerror("추출 데이터 파싱 오류", f"추출된 블록 데이터를 읽는 중 오류가 발생했습니다:\n{e}")
            return

        if extracted.get("status") != "OK":
            msg = extracted.get("message", "블록 선택이 취소되었습니다.")
            self.status_var.set(msg)
            return

        main_bname = extracted.get("main_block", "NEW_BLOCK")
        block_id = main_bname

        # 이미 동일한 이름의 블록이 있으면 덮어쓸지 새 이름으로 추가할지 확인
        if block_id in self.blocks_db:
            ans = messagebox.askyesno(
                "중복 블록 확인",
                f"이미 '{main_bname}' 블록이 라이브러리에 존재합니다.\n기존 데이터를 새로 추출한 블록으로 덮어쓰시겠습니까?"
            )
            if not ans:
                idx = 2
                while f"{main_bname}_{idx}" in self.blocks_db:
                    idx += 1
                block_id = f"{main_bname}_{idx}"

        extracted["checked"] = True
        extracted["settings"] = {
            "use_original_all": False,
            "block_name": block_id,
            "insert_layer": "[원본 유지 (Original)]",
            "insert_color": "[원본 유지 (Original)]",
            "insert_linetype": "[원본 유지 (Original)]",
            "entity_layer": "[원본 유지 (Original)]",
            "entity_color": "[원본 유지 (Original)]",
            "entity_linetype": "[원본 유지 (Original)]",
            "entity_textstyle": "[원본 유지 (Original)]",
            "textstyle_font": "romans.shx",
            "textstyle_bigfont": "whgtxt.shx",
        }

        self.blocks_db[block_id] = extracted
        self.current_block_id = block_id
        self.save_database()
        self._refresh_tree()
        self._load_block_to_form(block_id)
        self.status_var.set(f"성공: '{main_bname}' 블록을 추출하여 라이브러리에 저장했습니다!")

    # --------------------------------------------------------------------------
    # 기능 2: 선택한 블록을 현재 CAD 도면에 직접 붙여넣기 (SC / ROTATE 즉시 연동)
    # --------------------------------------------------------------------------
    def insert_selected_block_to_cad(self):
        if not self.current_block_id or self.current_block_id not in self.blocks_db:
            messagebox.showinfo("알림", "먼저 좌측 목록에서 CAD에 붙여넣을 블록을 선택해 주세요.")
            return

        # 현재 우측 폼의 최신 설정을 자동 반영
        self.save_current_block_settings(silent=True)

        acad, doc = self._get_acad_doc()
        if not doc:
            return

        rec = self.blocks_db[self.current_block_id]
        lisp_body = LispCodeGenerator.build_block_lisp_code(rec, include_commands=True)

        settings = rec.get("settings", {})
        target_name = rec["main_block"] if settings.get("use_original_all", False) else settings.get("block_name", rec["main_block"])
        func_sym = sanitize_symbol_name(target_name)

        # 로드 직후 바로 삽입 명령어가 실행되도록 트리거 추가
        full_lisp = lisp_body + f"\n(C:IB_{func_sym})\n"
        full_lisp = sanitize_for_cp949(full_lisp)

        tmp_lsp = os.path.join(self.base_dir, "tmp_cad_direct_insert.lsp")
        with open(tmp_lsp, "w", encoding="cp949", errors="replace") as f:
            f.write(full_lisp)

        try:
            acad.Visible = True
        except Exception:
            pass

        cmd = f'(load "{tmp_lsp.replace(chr(92), "/")}")\n'
        doc.SendCommand(cmd)
        self.status_var.set(f"AutoCAD 화면에서 '{target_name}' 블록의 삽입점을 클릭하세요! (배치 후 SC / RO 명령 즉시 사용 가능)")

    # --------------------------------------------------------------------------
    # 기능 3: 체크(☑)된 다중 블록들을 독립형 .lsp 파일로 내보내기
    # --------------------------------------------------------------------------
    def export_checked_blocks_to_lisp(self):
        # 현재 편집 중인 블록 설정 자동 반영
        if self.current_block_id:
            self.save_current_block_settings(silent=True)

        checked_ids = [bid for bid, rec in self.blocks_db.items() if rec.get("checked", True)]
        if not checked_ids:
            messagebox.showwarning("선택된 블록 없음", "내보낼 블록이 체크(☑)되어 있지 않습니다.\n좌측 목록에서 하나 이상의 블록을 체크해 주세요.")
            return

        save_path = filedialog.asksaveasfilename(
            title="독립형 블록 생성 리습(.lsp) 파일 저장",
            defaultextension=".lsp",
            initialfile="My_Block_Library.lsp",
            filetypes=[("AutoLISP 파일 (*.lsp)", "*.lsp"), ("모든 파일 (*.*)", "*.*")]
        )
        if not save_path:
            return

        # 기존 파일이 존재할 경우 .bak 백업 생성
        if os.path.exists(save_path):
            try:
                shutil.copy2(save_path, save_path + ".bak")
            except Exception:
                pass

        out_lines = []
        out_lines.append(";; ============================================================================")
        out_lines.append(";; AutoCAD 독립형 블록 자동 생성 및 삽입 라이브러리 (Auto-Generated LISP)")
        out_lines.append(f";; 수록된 블록 개수: {len(checked_ids)}개")
        out_lines.append(";; ----------------------------------------------------------------------------")
        out_lines.append(";; [사용 가능 명령어 요약]")
        out_lines.append(";;   1. LOAD_ALL_BLOCKS : 수록된 모든 블록 정의를 현재 도면에 일괄 등록")
        out_lines.append(";;   2. IB_블록이름     : 개별 블록을 즉시 생성 및 마우스 삽입 (삽입 후 SC/RO 연동)")
        out_lines.append(";; ============================================================================")
        out_lines.append("(vl-load-com)")
        out_lines.append("")

        create_func_calls = []
        cmd_names_info = []

        for bid in checked_ids:
            rec = self.blocks_db[bid]
            settings = rec.get("settings", {})
            t_name = rec["main_block"] if settings.get("use_original_all", False) else settings.get("block_name", rec["main_block"])
            f_sym = sanitize_symbol_name(t_name)

            code_chunk = LispCodeGenerator.build_block_lisp_code(rec, include_commands=True)
            out_lines.append(code_chunk)
            out_lines.append("")

            create_func_calls.append(f"  (_CREATE_BLK_{f_sym})")
            cmd_names_info.append(f"IB_{f_sym}")

        # 모든 블록 일괄 등록 명령어 (C:LOAD_ALL_BLOCKS) 추가
        out_lines.append(";; ============================================================================")
        out_lines.append(";; [통합 명령어] LOAD_ALL_BLOCKS : 파일 내 모든 블록을 현재 도면에 한 번에 생성/등록")
        out_lines.append(";; ============================================================================")
        out_lines.append("(defun C:LOAD_ALL_BLOCKS ( / )")
        out_lines.extend(create_func_calls)
        out_lines.append(f'  (princ "\\n[완료] 총 {len(checked_ids)}개의 블록 정의가 현재 도면에 등록되었습니다.")')
        out_lines.append("  (princ)")
        out_lines.append(")")
        out_lines.append("")
        out_lines.append(f'(princ "\\n[블록 라이브러리 로드 완료] 개별 삽입 명령어: {", ".join(cmd_names_info)} / 일괄 등록: LOAD_ALL_BLOCKS")')
        out_lines.append("(princ)")

        final_text = "\n".join(out_lines)
        final_text = sanitize_for_cp949(final_text)

        try:
            with open(save_path, "w", encoding="cp949", errors="replace") as f:
                f.write(final_text)
            self.status_var.set(f"리습 파일 생성 완료: {save_path} (총 {len(checked_ids)}개 블록)")
            messagebox.showinfo(
                "리습(.lsp) 파일 생성 완료",
                f"선택한 {len(checked_ids)}개의 블록이 포함된 독립형 리습 파일이 생성되었습니다!\n\n"
                f"▶ 저장 위치: {save_path}\n"
                f"▶ 개별 배치 명령어: {', '.join(cmd_names_info)}\n"
                f"▶ 전체 블록 도면 등록 명령어: LOAD_ALL_BLOCKS"
            )
        except Exception as e:
            messagebox.showerror("파일 저장 오류", f"리습 파일을 저장하는 중 오류가 발생했습니다:\n{e}")

    # --------------------------------------------------------------------------
    # 부가 기능: 단일 블록 LISP 코드 미리보기 팝업 & 블록 삭제
    # --------------------------------------------------------------------------
    def preview_single_lisp_code(self):
        if not self.current_block_id or self.current_block_id not in self.blocks_db:
            return
        self.save_current_block_settings(silent=True)
        rec = self.blocks_db[self.current_block_id]
        code_str = LispCodeGenerator.build_block_lisp_code(rec, include_commands=True)

        win = tk.Toplevel(self.root)
        win.title(f"AutoLISP 코드 미리보기 - {rec.get('main_block')}")
        win.geometry("820x600")

        top_bar = ttk.Frame(win, padding=6)
        top_bar.pack(fill=tk.X)

        def _copy_clip():
            win.clipboard_clear()
            win.clipboard_append(code_str)
            messagebox.showinfo("복사 완료", "AutoLISP 코드가 클립보드에 복사되었습니다.", parent=win)

        ttk.Button(top_bar, text="📋 클립보드에 전체 코드 복사", command=_copy_clip).pack(side=tk.LEFT)
        ttk.Label(top_bar, text="※ 이 코드를 기존 리습 파일에 붙여넣어 바로 사용할 수 있습니다.").pack(side=tk.LEFT, padx=10)

        txt = tk.Text(win, font=("Consolas", 10), wrap=tk.NONE)
        ys = ttk.Scrollbar(win, orient=tk.VERTICAL, command=txt.yview)
        xs = ttk.Scrollbar(win, orient=tk.HORIZONTAL, command=txt.xview)
        txt.configure(yscrollcommand=ys.set, xscrollcommand=xs.set)

        ys.pack(side=tk.RIGHT, fill=tk.Y)
        xs.pack(side=tk.BOTTOM, fill=tk.X)
        txt.pack(fill=tk.BOTH, expand=True)

        txt.insert("1.0", code_str)

    def delete_selected_block(self):
        # Ctrl/Shift 로 다중 선택한 항목 전체를 삭제 (선택이 없으면 현재 편집 중인 블록 1개)
        sel = [bid for bid in self.tree.selection() if bid in self.blocks_db]
        if not sel and self.current_block_id and self.current_block_id in self.blocks_db:
            sel = [self.current_block_id]
        if not sel:
            return
        if len(sel) == 1:
            msg = f"정말 '{sel[0]}' 블록을 라이브러리에서 삭제하시겠습니까?"
        else:
            msg = f"정말 선택한 {len(sel)}개 블록을 라이브러리에서 일괄 삭제하시겠습니까?"
        ans = messagebox.askyesno("삭제 확인", msg)
        if ans:
            for bid in sel:
                del self.blocks_db[bid]
            self.current_block_id = None
            self.save_database()
            self._refresh_tree()
            self.canvas.delete("all")
            self.status_var.set(f"{len(sel)}개 블록이 삭제되었습니다.")


# ==============================================================================
# 5. 프로그램 진입점 (더블클릭 실행 시 에러 로그 화면 유지 방어 로직 포함)
# ==============================================================================
def main():
    root = tk.Tk()
    app = BlockManagerApp(root)
    root.mainloop()


if __name__ == "__main__":
    try:
        main()
    except Exception:
        err_msg = traceback.format_exc()
        print("\n" + "=" * 70)
        print("[치명적 오류 발생] 프로그램 실행 중 예외가 발생했습니다 (상세 Traceback):")
        print("=" * 70)
        print(err_msg)
        print("=" * 70)
        try:
            err_root = tk.Tk()
            err_root.withdraw()
            messagebox.showerror("프로그램 실행 오류 (Traceback)", err_msg)
            err_root.destroy()
        except Exception:
            pass
        input("\n창을 닫으려면 Enter 키를 누르세요...")