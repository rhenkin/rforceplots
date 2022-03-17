import { reactWidget } from 'reactR';
import * as SHAP from "shapjs";

reactWidget(
  'ForcePlots',
  'output',
  {
    AdditiveForceVisualizer: SHAP.AdditiveForceVisualizer,
    AdditiveForceArrayVisualizer: SHAP.AdditiveForceArrayVisualizer,
    SimpleListVisualizer: SHAP.SimpleListVisualizer
}, {});
