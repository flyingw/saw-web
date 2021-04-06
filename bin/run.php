<?php
if (str_ends_with($_SERVER["SCRIPT_FILENAME"], ".css")) {
  header("content-type: text/css");
}
if (str_ends_with($_SERVER["SCRIPT_FILENAME"], ".js")) {
  header("content-type: text/javascript");
}
readfile($_SERVER["SCRIPT_FILENAME"]);
?>