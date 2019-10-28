/* z.geomap_country.js
----------------------------------------------------------

@author Marc Worrell <marc@worrell.nl>

Copyright 2019 Marc Worrell

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

---------------------------------------------------------- */

$.widget( "ui.geomap_country", {

    _init: function() {
        var self = this;
        var $elt = this.element;
        var i;

        z_ensure_id($elt);

        var max_zoom = self.optnum("maxzoom", self.options, 16);
        var map_zoom = self.optnum("zoom", self.options, 0);
        var map_location = [
            self.optnum("location_lng", self.options, 4.893715),
            self.optnum("location_lat", self.options, 52.372801)
        ];
        map_location = ol.proj.transform(map_location, 'EPSG:4326', 'EPSG:3857');
        var mapOptions = {
            target: $elt.attr('id'),
            theme: '/lib/css/ol.css',
            view: new ol.View({
                center: map_location,
                zoom: map_zoom,
                maxZoom: max_zoom
            }),
            layers: [
            ]
          };

        // create a vector layer for the world using a a topojson URL as the input
        // TODO: make countries clickable
        // https://stackoverflow.com/questions/29734606/how-to-use-different-color-for-topojson-objects-with-openlayers-3
        var style_world_border = new ol.style.Style({
            fill: new ol.style.Fill({
                color: 'rgba(0,0,0,0)'
            }),
            stroke: new ol.style.Stroke({
                color: 'rgba(80,80,80,1)',
                width: 1
            })
        });
        var vector_world = new ol.layer.Vector({
            source: new ol.source.Vector({
                url: '/lib/topojson/world-110m.json',
                format: new ol.format.TopoJSON({
                    layers: ['countries']
                }),
                overlaps: true
            }),
            style: style_world_border
        });

        mapOptions.layers.push(vector_world);

        new ol.Map(mapOptions);
    },

    optnum: function( n, opts, dflt ) {
        var v = opts[n];
        switch (typeof(v)) {
            case "string":
                if (isNaN(v) || v === "") {
                    return dflt;
                } else {
                    return parseFloat(v);
                };
            case "number":
                return v;
            default:
                return dflt;
        }
    }

});

$.ui.geomap_country.defaults = {
};
