import CCO.Component    (component, printer, ioWrap)
import CCO.GCL
import CCO.GCL.CodeTransform
import CCO.Tree         (fromTree, toTree, parser)
import Control.Arrow    (arr, (>>>))

main = ioWrap (parser >>> component toTree >>> arr transformProgs >>> arr renameVars >>> arr fromTree >>> printer)
