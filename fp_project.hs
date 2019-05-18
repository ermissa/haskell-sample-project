import Data.Fixed

floatMod :: Float -> Float -> Float
floatMod a b = a - (b * (fromIntegral $ truncate (a/b)))

rgb2hsv :: (Float,Float,Float) -> (Float,Float,Float)
rgb2hsv (r,g,b) = (h,s,v) where
    r_scale = r / 255
    g_scale = g / 255
    b_scale = b / 255
    cmax = max (max r_scale g_scale) b_scale
    cmin = min (min r_scale g_scale) b_scale
    delta = cmax - cmin
    h
        | delta == 0 = 0
        | cmax == r_scale = 60 * ( floatMod ((g_scale-b_scale) / delta) 6 )
        | cmax == g_scale = 60 * ( (b_scale-r_scale) / delta + 2 )
        | cmax == b_scale = 60 * ( (r_scale-g_scale) / delta + 4 )
    s
        | cmax == 0 = 0
        | cmax /= 0 = (delta / cmax)
    
    v = cmax


hsv2rgb :: (Float,Float,Float) -> (Float,Float,Float)
hsv2rgb (h,s,v) = (r,g,b) where
    c = v * s
    x = c * (1- abs (( floatMod (h / 60) 2) - 1 ))
    m = v - c
    (r_scale,g_scale,b_scale)
        | h >= 0 && h < 60 = (c,x,0)
        | h >= 60 && h < 120 = (x,c,0)
        | h >= 120 && h < 180 = (0,c,x)
        | h >= 180 && h < 240 = (0,x,c)
        | h >= 240 && h < 300 = (x,0,c)
        | h >= 300 && h < 360 = (c,0,x)
    r = (( r_scale + m ) * 255)
    g = (( g_scale + m ) * 255)
    b = (( b_scale + m ) * 255)


name2rgb :: String -> (Float,Float,Float)
name2rgb htmlcolorname
        | htmlcolorname == "AliceBlue" = (240, 248, 255)
        | htmlcolorname == "AntiqueWhite" = (250, 235, 215)
        | htmlcolorname == "Aqua" = (0, 255, 255)
        | htmlcolorname == "Aquamarine" = (127, 255, 211)
        | htmlcolorname == "Azure" = (240, 255, 255)
        | htmlcolorname == "Beige" = (245, 245, 220)
        | htmlcolorname == "Bisque" = (255, 227, 196)
        | htmlcolorname == "Black" = (0, 0, 0)
        | htmlcolorname == "Chocolate" = (210, 105, 29)
        | htmlcolorname == "LightSkyBlue" = (135, 205, 250)
        | htmlcolorname == "Violet" = (245, 222, 179)
        | htmlcolorname == "SpringGreen" = (0, 255, 127)

        
hsv2desc :: (Float,Float,Float) -> String
hsv2desc (h,s,v) = description where
    hue 
        | h > 344 = "red"
        | h > 327 = "rose"
        | h > 291 = "magenta"
        | h > 270 = "purple"
        | h > 260 = "violet"
        | h > 240 = "indigo"
        | h > 193 = "blue"
        | h > 163 = "cyan"
        | h >  79 = "green"
        | h >  70 = "lime"
        | h >  45 = "yellow"
        | h >  15 = "orange"
        | h == 15 = "reddish"
        | h <  15 = "red"

    saturation
        | s >  90 = "very saturated"
        | s >  80 = "rather saturated"
        | s >  60 = "saturated"
        | s >  46 = "rather unsaturated"
        | s >  30 = "unsaturated"
        | s >  10 = "very unsaturated"
        | s >  3  = "almost grey"
        | s <  4  = "grey"

    value
        | v > 94 = "almost white"
        | v > 80 = "very light"
        | v > 60 = "light"
        | v > 30 = "normal"
        | v > 22 = "dark"
        | v >  9 = "very dark"
        | v < 10 = "almost black"

    description = saturation ++ " " ++ value ++ " " ++ hue

main = do
    print(rgb2hsv(128,128,0))
    print(hsv2rgb(300,1,0.5))
    print(name2rgb("AliceBlue"))
    print(hsv2desc(260,11,100))