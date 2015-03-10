import CCO.Component    (component, printer, ioWrap)
import CCO.GCL
import CCO.Tree         (fromTree, toTree, parser)
import Control.Arrow    (arr, (>>>))

main = ioWrap (parser >>> component toTree >>> arr head >>> arr renameVars >>> arr wlp >>> arr fromTree >>> printer)
