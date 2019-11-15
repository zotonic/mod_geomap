/* z.geomap.js
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

(function() {

    $.widget( "ui.geomap", {

        _init: function() {
            var self = this;
            var $elt = this.element;
            var i;

            z_ensure_id($elt);

            var tile_url = self.options.tile_url || 'https://{a-c}.tile.openstreetmap.de/{z}/{x}/{y}.png';
            var max_zoom = self.optnum("maxzoom", self.options, 28);
            var map_zoom = self.optnum("zoom", self.options, 9);
            var map_location = [
                self.optnum("location_lng", self.options, 4.893715),
                self.optnum("location_lat", self.options, 52.372801)
            ];
            map_location = ol.proj.transform(map_location, 'EPSG:4326', 'EPSG:3857');
            var mapOptions = {
                target: $elt.attr('id'),
                theme: '/lib/css/ol.css',
                // controls: ol.control.defaults().extend([
                //     new ol.control.FullScreen()
                // ]),
                view: new ol.View({
                    center: map_location,
                    zoom: map_zoom,
                    maxZoom: max_zoom
                }),
                layers: [
                    new ol.layer.Tile({
                        source: new ol.source.OSM({
                            url: tile_url
                        })
                    })
                  ]
              };

            var features = [];
            if (self.options.locations) {
                var styleCache = {};
                var locs = self.options.locations;
                var loc_ct = locs.length;
                for (i = 0; i < loc_ct; i++) {
                    let coordinates = [ parseFloat(locs[i].lng), parseFloat(locs[i].lat) ];
                    if (!isNaN(coordinates[0]) && !isNaN(coordinates[1])) {
                        var feature = new ol.Feature({
                            geometry: new ol.geom.Point(ol.proj.transform(coordinates, 'EPSG:4326', 'EPSG:3857'))
                        });
                        feature.setId(i);
                        features.push(feature)
                    }
                }
                var source = new ol.source.Vector({
                    features: features
                });

                var clusterSource = new ol.source.Cluster({
                    distance: self.options.clusterDistance ? self.options.clusterDistance : 20,
                    source: source
                });

                // TODO: use the locations "icon" (if count == 1)
                var clusters = new ol.layer.Vector({
                  source: clusterSource,
                  style: function(feature) {
                    var size = feature.get('features').length;
                    var styleKey = size;
                    var styleIcon = false;

                    if (size == 1) {
                        var fid = feature.get('features')[0].getId();
                        if (typeof fid !== 'undefined') {
                            var loc = self.options.locations[fid];
                            if (loc.icon) {
                                styleIcon = loc.icon;
                                styleKey = loc.icon;
                            }
                        }
                    }
                    var style = styleCache[styleKey];
                    if (!style) {
                        if (styleIcon) {
                            style = new ol.style.Style ({
                                image: new ol.style.Icon({
                                    src: styleIcon,
                                    anchor: [ 0.5, 0.98 ]
                                })
                            });
                        } else {
                            var radius = 10;
                            var clusterBgColor = self.options.clusterBgColor ? self.options.clusterBgColor : '#3399CC';
                            var clusterColor = self.options.clusterColor ? self.options.clusterColor : '#fff';
                            if (size >= 100) radius = 14;
                            else if (size >= 10) radius = 12;
                            style = new ol.style.Style({
                                image: new ol.style.Circle({
                                    radius: radius,
                                    stroke: new ol.style.Stroke({
                                        color: clusterColor
                                    }),
                                    fill: new ol.style.Fill({
                                        color: clusterBgColor
                                    })
                                }),
                                text: new ol.style.Text({
                                    text: size.toString(),
                                    fill: new ol.style.Fill({
                                        color: clusterColor
                                    })
                                })
                            });
                        }
                        styleCache[styleKey] = style;
                    }
                    return style;
                  }
                });
                mapOptions.layers.push(clusters);
            }

            // Optional marker on the map center, without click handler
            if (self.options.marker) {
                var pin_location = [
                    self.optnum("location_lng", self.options, undefined),
                    self.optnum("location_lat", self.options, undefined)
                ];
                if (!isNaN(pin_location[0]) && !isNaN(pin_location[1])) {
                    mapOptions.layers.push(
                        new ol.layer.Vector ({
                          source: new ol.source.Vector ({
                            features: [
                                new ol.Feature(new ol.geom.Point(
                                    ol.proj.transform(pin_location, 'EPSG:4326', 'EPSG:3857')
                                ))
                            ]
                          }),
                          style: new ol.style.Style ({
                            image: new ol.style.Icon({
                              src: '/lib/images/marker-default.png',
                              anchor: [ 0.5, 0.98 ]
                            })
                          })
                        })
                    );
                }
            }

            var map = new ol.Map(mapOptions);

            function isCluster(feature) {
                if (!feature || !feature.get('features')) {
                    return false;
                }
                return feature.get('features').length > 1;
            }

            function showInfoBox(features) {
                var ids = [];
                var data = [];

                for (i = 0; i < features.length; i++) {
                    var fid = features[i].getId();
                    if (typeof(fid) !== 'undefined')
                    {
                        var loc = self.options.locations[fid];
                        ids.push(loc.id);
                        data.push(loc.data);
                    }
                }
                z_event('map_infobox', {ids: ids, element: $elt.get('id'), data: data});
            }

            function boundingGeom(features) {
                var coords = features.map(function(x) { return x.getGeometry().getCoordinates(); });
                var maxX = coords.map(function(x) { return x[0]; }).reduce(function(a, b) {
                    return Math.max(a, b);
                });
                var maxY = coords.map(function(x) { return x[1]; }).reduce(function(a, b) {
                    return Math.max(a, b);
                });
                var minX = coords.map(function(x) { return x[0]; }).reduce(function(a, b) {
                    return Math.min(a, b);
                });
                var minY = coords.map(function(x) { return x[1]; }).reduce(function(a, b) {
                    return Math.min(a, b);
                });

                var d = (29 - map.getView().getZoom()) * 50;
                var delta = Math.max(maxX - minX,  maxY - minY);
                var d_max = (29 - map.getView().getMaxZoom()) * 50;

                return {
                    is_zoomable: d_max < delta,
                    delta: delta,
                    geometry: new ol.geom.MultiPoint([ [maxX+d, maxY+d], [minX-d, minY-d] ])
                };
            }

            if (features.length) {
                map.on("click", function(evt) {
                    var feature = map.forEachFeatureAtPixel(evt.pixel, function(feature) { return feature; });
                    if (feature) {
                        if (   typeof(feature.get) == 'function'
                            && feature.get('features'))
                        {
                            var fs = feature.get('features');
                            var g = boundingGeom(fs);
                            if (   fs.length > 1
                                && map.getView().getZoom() < map.getView().getMaxZoom()
                                && g.is_zoomable) {
                                map.getView().fit(g.geometry, map.getSize())
                            } else {
                                showInfoBox(fs);
                            }
                        }
                    }
                });

                var g = boundingGeom(features);
                map.getView().fit(g.geometry, map.getSize());
            }
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
})();

$.ui.geomap.defaults = {
};
