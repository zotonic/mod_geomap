/* z.geomap.js
----------------------------------------------------------

@author Driebit BV

Copyright 2019 Driebit BV

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

$.widget( "ui.geomap", {

    _create: function() {
        var self = this,
            widgetElement = $(self.element),
            id = widgetElement.attr('id'),
            locations = self.options.locations,
            options,
            map,
            bounds = new google.maps.LatLngBounds(),
            icon,
            i,
            mc,
            mcOptions;
        var map_options = {};

        z_ensure_id(widgetElement);

        markers = [];
        self.id = id;
        options = self.options;
        locations = self.options.locations;

        var styles = [];
        var stylers = [];

        styles.push({"stylers":stylers});

        map_options.styles = styles;

        if (options.blackwhite == true) {
            stylers.push({ "saturation": -100 });
            stylers.push({ "lightness": -8 });
            stylers.push({ "gamma": 1.18 });
        }
        if (options.mapstyle) {
            if (Array.isArray(options.mapstyle)) {
                styles = $.merge(styles, options.mapstyle);
            } else {
                styles.push(options.mapstyle);
            }
        }
        if (options.disabledefaultui) {
            map_options.disableDefaultUI = true;
        }
        mcOptions = {
                styles: [
                {
                    height: 28,
                    width: 28,
                    url: "/lib/images/cluster-default-small.svg"
                },
                {
                    height: 45,
                    width: 45,
                    url: "/lib/images/cluster-default.svg"
                },
                {
                    height: 54,
                    width: 54,
                    url: "/lib/images/cluster-default-large.svg"
                }
            ],
            zoomOnClick: false
        };

        if (options.gridsize) {
            mcOptions.gridSize = parseInt(options.gridsize);
        }

        map = new google.maps.Map(document.getElementById(id), map_options);

        self.map = map;
        self.infowindow = null;
        self.markers = markers;

        // Show multiple markers with info windows
        for (i = 0; i < locations.length; i++) {
            if (locations[i].icon) {
                icon = '/image/'+locations[i].icon;
            } else {
                icon = '/lib/images/marker-default.png';
            }
            var marker = new google.maps.Marker({
                position: new google.maps.LatLng(locations[i].lat, locations[i].lng),
                icon: icon,
                zotonic_id: parseInt(locations[i].id),
                data: locations[i].data
            });
            marker.addListener('click', function() {
                var markerList = [];
                markerList.push(this);
                self.startShowInfoWindow(markerList);
            });
            bounds.extend(marker.position);
            markers.push(marker);
        }

        // Optional marker on the map center, without click handler
        if (options.marker) {
            if (   options.location_lat && !isNaN(options.location_lat)
                && options.location_lng && !isNaN(options.location_lng))
            {
                var marker = new google.maps.Marker({
                    position: new google.maps.LatLng(
                        parseFloat(options.location_lat),
                        parseFloat(options.location_lng)),
                    icon: "/lib/images/marker-default.png"
                });
                bounds.extend(marker.position);
                markers.push(marker);
            }
        }

        mc = new MarkerClusterer(map, markers, mcOptions);
        self.mc = mc;

        google.maps.event.addListener(mc, "clusterclick", function(cluster) {
            $.proxy(self.clusterClicked(cluster), self);
            return false;
        });

        var maxZoom = (!options.maxzoom) ? 15 : parseInt(options.maxzoom);
        google.maps.event.addListener(map, 'bounds_changed', function(event) {
            if ( this.getZoom() > maxZoom ){
                this.setZoom(maxZoom);
            }
        });

        if (options.loadgeojson && options.datastyle) {
            map.data.loadGeoJson(options.loadgeojson);
            map.data.setStyle(options.datastyle);
        }
        map.fitBounds(bounds);
    },

    clusterClicked: function(cluster) {
        var self = this,
            markers = cluster.getMarkers(),
            posCoordList = [],
            markerList = [],
            zoom = self.map.getZoom(),
            clusterBounds = new google.maps.LatLngBounds();

        $.each(markers, function(index, marker) {
            clusterBounds.extend(marker.position);
            posCoordList.push(marker.position.lat() + ', ' + marker.position.lng());
            markerList.push(marker);
        });
        posCoordList = self.unique(posCoordList);
        if (posCoordList.length == 1 || zoom >= 15) {
            $.proxy(self.startShowInfoWindow(markerList), self);
            return false;
        } else {
            self.map.fitBounds(clusterBounds);
        }
    },

    unique: function(list) {
      var result = [];
      $.each(list, function(i, e) {
          if ($.inArray(e, result) == -1) result.push(e);
      });
      return result;
    },

    startShowInfoWindow: function(markerList) {
        var self = this;
        var ids = $.map(markerList, function(val, i) {
            return val.zotonic_id;
        });
        var data = $.map(markerList, function(val, i) {
            return val.data;
        });
        z_event('map_infobox', {ids: ids, element: self.id, data: data});
    },

    showInfoWindow: function(zotonic_id, contentHTML) {
      var self = this,
          marker = self.getMarker(zotonic_id),
          ibOptions = {
            content: contentHTML,
            disableAutoPan: false,
            maxWidth: 0,
            maxHeight: 200,
            pixelOffset: new google.maps.Size(-140, 0),
            zIndex: null,
                closeBoxURL: "/lib/images/infobox-close.svg",
            infoBoxClearance: new google.maps.Size(1, 1),
            isHidden: false,
            pane: "floatPane",
            enableEventPropagation: false
        },
        offsetX = self.options.panOffsetX,
        offsetY = self.options.panOffsetY,
        scale = Math.pow(2,self.map.getZoom()),
        center = self.map.getProjection().fromLatLngToPoint(marker.getPosition()),
        newCenterPoint = new google.maps.Point(center.x - offsetX/scale, center.y + offsetY/scale),
        newCenter = self.map.getProjection().fromPointToLatLng(newCenterPoint);

        self.map.set('scrollwheel', false);
        self.map.panTo(newCenter);
        if (self.infowindow) {
            self.infowindow.close();
        }
        self.infowindow = new InfoBox(ibOptions);
        self.infowindow.open(self.map, marker);

        google.maps.event.addListener(self.infowindow,'closeclick',function(){
           self.map.set('scrollwheel', true);
        });
    },

    getMarker: function(zotonic_id) {
      var marker;

      $.each(this.markers, function(i, val) {
        if (val.zotonic_id == zotonic_id) marker = val;
      });
      return marker;
    },

    enableUI: function() {
        this.map.set('disableDefaultUI', false);
    },

    removeStyles: function() {
        this.map.set('styles', '');
    },

    triggerResize: function() {
        google.maps.event.trigger(this.map, "resize");
    }

});
