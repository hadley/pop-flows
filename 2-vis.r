library(qtpaint)

# County boundaries
if (!exists("boundary")) {
  boundary <- read.csv("boundaries.csv.bz2")
  # Remove alaska and hawawii for now
  boundary <- subset(boundary, !(state %in% c(2, 15)))

  polys <- split(boundary, boundary$group)  
}

render_borders <- function(item, painter, exposed) { 
  qstrokeColor(painter) <- "grey70" 
  for(poly in polys) {
    qdrawPolygon(painter, poly$long, poly$lat)
  }
}

render_highlight <- function(item, painter, exposed) { 
  h_poly <- polys[[highlighted + 1]]
  qdrawPolygon(painter, h_poly$long, h_poly$lat, 
    stroke = "NA", fill = "grey70")

  s_poly <- polys[[selected + 1]]
  qdrawPolygon(painter, s_poly$long, s_poly$lat, 
    stroke = "black", fill = "grey50")
}

highlighted <<- NA
selected <<- NA

hover_county <- function(layer, event) {
  mat <- layer$deviceTransform(event)$inverted()

  rect <- qrect(-1, -1, 1, 1)
  rect <- mat$mapRect(rect) # now in data space
  pos <- event$pos()
  rect$moveCenter(pos) # centered on the pointer data pos
  highlighted <<- layer$locate(rect)[1] # get indices in rectangle
  
  qupdate(highlight)
}

select_county <- function(layer, event) {
  selected <<- highlighted
}

if (exists("view")) view$close()

scene <- Qt$QGraphicsScene()
root <- qlayer(scene)
view <- qplotView(scene = scene)

borders <- qlayer(root, render_borders, 
  hoverMoveEvent = hover_county, mousePressFun = select_county)
borders$setLimits(qrect(range(boundary$long), range(boundary$lat)))

highlight <- qlayer(root, render_highlight)
highlight$setLimits(borders$limits())

print(view)
