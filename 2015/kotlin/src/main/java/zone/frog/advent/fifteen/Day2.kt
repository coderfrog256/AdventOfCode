package zone.frog.advent.fifteen

fun getSquareFeet(l: Int, w: Int, h: Int) =
    minOf(l*w, w*h, l*h)
        .let { it + 2*l*w + 2*w*h + 2*l*h  }

fun getRibbonLength(l: Int, w: Int, h: Int) =
    minOf(l+l+w+w, w+w+h+h, h+h+l+l) + l*w*h