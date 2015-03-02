import CCO.Component    (component, printer, ioWrap)
import CCO.GCL          (wlp)
import CCO.Tree         (fromTree, toTree, parser)
import Control.Arrow    (arr, (>>>))

main = ioWrap (parser >>> component toTree >>> arr wlp >>> arr fromTree >>> printer)
