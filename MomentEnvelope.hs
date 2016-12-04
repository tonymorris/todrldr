import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour.SRGB

_inner :: [Double] -> [(Double,Double)]
_inner xs = [ (x,(25*x+25)) | x <- xs ]

_bottom_left :: [Double] -> [(Double,Double)]
_bottom_left xs = [ (x,(900/31)*x-(750/31)) | x <- xs ]

_top_left :: [Double] -> [(Double,Double)]
_top_left xs = [ (x,(1200/73)*x+(60750/73)) | x <- xs ]

_right :: [Double] -> [(Double,Double)]
_right xs = [ (x,(2100/99)*x-(600/99)) | x <- xs ]

peach :: AlphaColour Double
peach = opaque $ sRGB 1 0.95 0.9

main = toFile def "moment_envelope.png" $ do

    -- Set the background colours.
    layout_background .= (FillStyleSolid $ opaque white)
    layout_plot_background .= Just (FillStyleSolid $ opaque white)

    -- Set the margin.
    layout_margin .= 20

    -- Set titles.
    layout_title .= "CENTER OF GRAVITY MOMENT ENVELOPE"
    layout_x_axis . laxis_title .= "Loaded Airplane Moment/1000 (Pounds - Inches)"
    layout_y_axis . laxis_title .= "Loaded Airplane Weight (Pounds)"

    -- Extend the range of the y-axis. Note that the range we specify
    -- here is may be extended to the nearest tick-mark. For full
    -- control, over the range and scaling, supply an AxisFn.
    layout_y_axis . laxis_generate .= scaledAxis def (1500, 2600)

    -- Format the main title
    layout_title_style . font_size .= 20
    layout_title_style . font_weight .= FontWeightBold
    -- see also font_name, font_slant, font_color

    -- Format other titles
    layout_x_axis . laxis_title_style . font_size .= 16
    layout_x_axis . laxis_style . axis_label_style . font_size .= 14
    layout_y_axis . laxis_title_style . font_size .= 16
    layout_y_axis . laxis_style . axis_label_style . font_size .= 14
    -- see also font_name, font_weight, font_slant, font_color

    -- Show the top and right axes
    layout_top_axis_visibility . axis_show_line .= True
    layout_top_axis_visibility . axis_show_ticks .= True
    layout_right_axis_visibility . axis_show_line .= True
    layout_right_axis_visibility . axis_show_ticks .= True
    -- also see layout_bottom_axis_visibility, layout_left_axis_visibility

    -- Turn off the legend.
    layout_legend .= Nothing

    plot (line "_inner" [_inner [50,51..87]])
    plot (line "_bottom_left" [_bottom_left [45,51..68]])
    plot (line "_top_left" [_top_left [68,69..104]])
    plot (line "_right" [_right [50,51..120]])