import CCO.Component    (printer, ioWrap)
import CCO.GCL.Parser   (parser)
import CCO.GCL          (renameVars)
import CCO.Tree         (fromTree)
import Control.Arrow    (arr, (>>>))

main = ioWrap (parser >>> arr renameVars >>> arr fromTree >>> printer)
