library(qtpaint)

# County boundaries
if (!exists("boundary")) {
  boundary <- read.csv("boundaries.csv.bz2")
  # Remove alaska and hawawii for now
  boundary <- subset(boundary, !(state %in% c(2, 15)))

  polys <- split(boundary, boundary$group)
  source("poly.r")
  centers <- plyr::ddply(boundary, c("state", "county"), info)
  
  flow <- read.csv("flow.csv")
  flow <- merge(flow, centers, by.x = c("state_to", "county_to"),
    by.y = c("state", "county"))
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
  qupdate(flow_layer)
}

render_flow <- function(item, painter, exposed) {
  if (is.na(selected)) return()
  county <- as.list(polys[[selected + 1]][1, 1:2])
  
  movement <- subset(flow, state_from == county$state & 
    county_from == county$county)
  movement$size <- sqrt(abs(movement$change) / max(abs(movement$change)))
  
  flow_in <- subset(movement, change > 0)
  flow_out <- subset(movement, change < 0)
  
  circle <- qglyphCircle(2)
  
  qdrawGlyph(painter, circle, flow_in$long, flow_in$lat, 
    stroke = "NA", fill = "black", cex = 3 * flow_in$size + 1)
  qdrawGlyph(painter, circle, flow_out$long, flow_out$lat, 
    stroke = "NA", fill = "red", cex = 3 * flow_out$size + 1)
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

flow_layer <- qlayer(root, render_flow)
flow_layer$setLimits(borders$limits())


print(view)
