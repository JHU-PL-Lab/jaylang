type ddpa_logging_level =
  | Log_nothing
  | Log_result
  | Log_everything
  [@@deriving eq, ord, show, to_yojson]
;;

type ddpa_analysis_logging_config =
  { ddpa_json_logger : Yojson.Safe.json -> unit
  ; ddpa_cfg_logging_level : ddpa_logging_level
  ; ddpa_pdr_logging_level : ddpa_logging_level
  ; ddpa_pdr_deltas : bool
  }
  [@@deriving show]
;;
