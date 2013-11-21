/* Shiny output binding to Leaflet map
See: 

 - Write an Output Binding
   http://rstudio.github.com/shiny/tutorial/#building-outputs

 - shiny-d3-calendar
   http://rstudio.github.com/shiny/tutorial/#building-outputs. 
*/ 

/* Leaflet map */

var map = L.map('map').setView([0, 0], 2); // L.map('map',{crs: L.CRS.EPSG4326, worldCopyJump: true})

var cloudmade = L.tileLayer('http://{s}.tile.cloudmade.com/{key}/{styleId}/256/{z}/{x}/{y}.png', {
	attribution: 'CloudMade',
	key: 'BC9A493B41014CAABB98F0471D759707',
	styleId: 22677
}).addTo(map);


// get field value from drop-down with id 'var'
fld = document.getElementById('var').value // 'alien_species';
fld_arr = ohiRegions.features.map(function(element, index, array){ return element.properties[fld] })
fld_min = Math.min.apply(Math, fld_arr)
fld_max = Math.max.apply(Math, fld_arr)
fld_incr = (fld_max - fld_min) / 8
var fld_incr_arr = [fld_min];
for (i=1; i<=7; i++){
	fld_incr_arr.push(fld_min + i * fld_incr)
};

// control that shows state info on hover
var info = L.control();
info.onAdd = function (map) {
	this._div = L.DomUtil.create('div', 'info');
	this.update();
	return this._div;
};
info.update = function (props) {
	this._div.innerHTML = '<h4>OHI Inspector</h4>' +  (props ?
		props['region_id'] + ': <b>' + props[fld] + '</b>'
		: 'Hover over a region');
};
info.addTo(map);

// get color depending on population density value
function getColor(d) {
	return d > (fld_min + fld_incr * 7) ? '#800026' :
	       d > (fld_min + fld_incr * 6) ? '#BD0026' :
	       d > (fld_min + fld_incr * 5) ? '#E31A1C' :
	       d > (fld_min + fld_incr * 4) ? '#FC4E2A' :
	       d > (fld_min + fld_incr * 3) ? '#FD8D3C' :
	       d > (fld_min + fld_incr * 2) ? '#FEB24C' :
	       d > (fld_min + fld_incr * 1) ? '#FED976' :
	                  		              '#FFEDA0';
}

function style(feature) {
	return {
		weight: 2,
		opacity: 1,
		color: 'white',
		dashArray: '3',
		fillOpacity: 0.7,
		fillColor: getColor(feature.properties[fld])
	};
}

function highlightFeature(e) {
	var layer = e.target;

	layer.setStyle({
		weight: 5,
		color: '#666',
		dashArray: '',
		fillOpacity: 0.7
	});

	if (!L.Browser.ie && !L.Browser.opera) {
		layer.bringToFront();
	}

	info.update(layer.feature.properties);
}

function resetHighlight(e) {
	geojson.resetStyle(e.target);
	info.update();
}

function zoomToFeature(e) {
	map.fitBounds(e.target.getBounds());
}

function onEachFeature(feature, layer) {
	layer.on({
		mouseover: highlightFeature,
		mouseout: resetHighlight,
		click: zoomToFeature
	});
}

geojson = L.geoJson(ohiRegions, {
	style: style,
	onEachFeature: onEachFeature
}).addTo(map);
map.attributionControl.addAttribution('<a href="http://www.oceanhealthindex.org/">Ocean Health Index</a>');

var legend = L.control({position: 'bottomright'});
legend.onAdd = function (map) {

	var div = L.DomUtil.create('div', 'info legend'),
		grades = fld_incr_arr,			
		labels = ['<b>'+fld+'</b>'],
		from, to;

	for (var i = 0; i < grades.length; i++) {
		from = grades[i];
		to = grades[i + 1];

		labels.push(
			'<i style="background:' + getColor(from) + '"></i> ' +
			from + (to ? '&ndash;' + to : '&ndash;' + fld_max));
	}

	div.innerHTML = labels.join('<br>');
	return div;
};
legend.addTo(map);

/* Shiny bindings */

var mapOutputBinding = new Shiny.OutputBinding();
$.extend(mapOutputBinding, {
	find: function(scope) {
		return $(scope).find('.shiny-map-output');
	},
	renderValue: function(el, data) {
		fld = data;
		// redraw map and legend based on chosen variable

		fld_arr = ohiRegions.features.map(function(element, index, array){ return element.properties[fld] })
		fld_min = Math.min.apply(Math, fld_arr)
		fld_max = Math.max.apply(Math, fld_arr)
		fld_incr = (fld_max - fld_min) / 8
		var fld_incr_arr = [fld_min];
		for (i=1; i<=7; i++){
			fld_incr_arr.push(fld_min + i * fld_incr)
		};

		legend.removeFrom(map)
		legend.addTo(map)
		geojson.clearLayers()
		geojson = L.geoJson(ohiRegions, {
					style: style,
					onEachFeature: onEachFeature
				}).addTo(map);
	} // end renderValue: function(el, data) {
}); // end $.extend(networkOutputBinding, {
	
Shiny.outputBindings.register(mapOutputBinding, 'shiny-map-output');