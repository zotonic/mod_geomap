{# This template is lazy loaded from the _geomap_admin_location.tpl #}

<input id="location_zoom_level" type="hidden" name="location_zoom_level" value="{{ m.rsc[id].location_zoom_level }}" />

{% with m.rsc[id].computed_location_lat as latitude %}
{% with m.rsc[id].computed_location_lng as longitude %}
{% with m.rsc[id].location_zoom_level|default:"15" as zoom_level %}
<div class="row">
    <div class="form-group col-md-6">
    	<label for="location_lat" class="control-label">{_ Latitude _}</label>
		<input id="location_lat" type="text" name="location_lat" value="{{ m.rsc[id].location_lat }}" class="form-control" />
        {% if id %}
            <span class="text-muted">{_ indexed _}: {{ latitude|default:"-" }}</span>
        {% endif %}
    </div>

    <div class="form-group col-md-6">
    	<label for="location_lng" class="control-label">{_ Longitude _}</label>
    	<div class="controls">
    		<input id="location_lng" type="text" name="location_lng" value="{{ m.rsc[id].location_lng }}" class="form-control" />
            {% if id %}
        		<span class="text-muted">{_ indexed _}: {{ longitude|default:"-" }}</span>
            {% endif %}
    	</div>
    </div>
</div>

<div class="form-group">
	<button class="btn" id="location_me"><i class="icon-screenshot"></i> {_ Set to current location _}</button>
	<button class="btn" id="location_address"><i class="icon-screenshot"></i> {_ Set to entered address _}</button>
	<button class="btn" id="location_clear">{_ Clear _}</button>
	<button class="btn" id="location_reset">{_ Reset _}</button>
</div>

<div id="{{ #geomap }}" class="admin-geomap map" style="height: 480px; margin-bottom:8px"></div>

<p class="text-muted"><span class="fa fa-info-circle"></span> {_ Please click on the map to select the location. _}</p>

{% javascript %}
{# Hide the 'entered address' button if there is no address select field on the edit page #}
if ($('#address_country').length == 0) {
    $('#location_address').hide();
}

{# Initialize the map, with a timeout function so that the interface works before the map is loaded #}

var map;
var map_geolocate;
var map_pin_layer_feature;

setTimeout(function() {

    /* Center the map on the current (calculated) position */
    {% if longitude|is_defined and latitude|is_defined %}
        var map_location = [
            {{ longitude }},
            {{ latitude }}
        ];
        var map_zoom = parseFloat("{{ m.rsc[id].location_zoom_level|default:zoom_level }}");
        var has_pin = true;
    {% else %}
        var map_location = [
            parseFloat("{{ m.config.mod_geomap.location_lng.value|default:0 }}"),
            parseFloat("{{ m.config.mod_geomap.location_lat.value|default:0 }}")"
        ];
        var map_zoom = 2;
        var has_pin = false
    {% endif %}
    map_location = ol.proj.transform(map_location, 'EPSG:4326', 'EPSG:3857');

    var mapOptions = {
        target: '{{ #geomap }}',
        theme: '/lib/css/ol.css',
        view: new ol.View({
            center: map_location,
            zoom: map_zoom
        }),
        layers: [
            new ol.layer.Tile({
              source: new ol.source.OSM()
            })
          ]
      };

    map = new ol.Map(mapOptions);

    if (has_pin) {
        map_pin_layer_feature = new ol.Feature(new ol.geom.Point(map_location))
        map.addLayer(
            new ol.layer.Vector ({
              source: new ol.source.Vector ({
                features: [ map_pin_layer_feature ]
              }),
              style: new ol.style.Style ({
                image: new ol.style.Icon({
                  src: '/lib/images/marker.png',
                  anchor: [ 0.5, 0.98 ]
                })
              })
            })
        );
    }

    /* Geolocate handler - to show current location */
    map_geolocate = new ol.Geolocation({
        trackingOptions: {
            enableHighAccuracy: true
        },
        projection: mapOptions.view.getProjection()
    });
    map_geolocate.on('change:position', function() {
        // 52.319567933090994
        // 4.864136531278204
        var coordinate = map_geolocate.getPosition();
        map_geolocate.setTracking(false);
        coordinate = ol.proj.transform(coordinate, 'EPSG:3857', 'EPSG:4326');
        map_mark_location(coordinate[0], coordinate[1], 'gps');
    });

    /* Handle click on map to set the position */
    map.on('singleclick', function(evt) {
        var coordinate = evt.coordinate;
        coordinate = ol.proj.transform(coordinate, 'EPSG:3857', 'EPSG:4326');
        map_mark_location(coordinate[0], coordinate[1], 'click');
    });

}, 100);

$('#location_me').click(function(ev) {
    map_geolocate.setTracking(true);
    $(this).addClass('disabled');
    ev.preventDefault();
});
$('#location_lat, #location_lng').on("change keyup", function() {
    var latitude = parseFloat( $('#location_lat').val() );
    var longitude = parseFloat( $('#location_lng').val() );
    if (!isNaN(latitude) && !isNaN(longitude)) {
        map_mark_location(longitude, latitude, 'typing');
    }
});
$('#location_address').click(function(ev) {
    if ($('#address_country').length > 0) {
        var args = {
            street: $('#address_street_1').val(),
            city: $('#address_city').val(),
            postcode: $('#address_postcode').val(),
            state: $('#address_state').val(),
            country: $('#address_country').val(),
            z_delegate: 'mod_geomap'
        };
        z_notify("address_lookup", args);
        $(this).addClass('disabled');
    }
    ev.preventDefault();
});
$('#location_clear').click(function(ev) {
    $('#location_lat').val('');
    $('#location_lng').val('');
    markers.clearMarkers();
    ev.preventDefault();
});
$('#location_reset').click(function(ev) {
    $('#location_lat').val('{{ m.rsc[id].location_lat }}');
    $('#location_lng').val('{{ m.rsc[id].location_lng }}');
    var latitude = parseFloat('{{ m.rsc[id].computed_location_lat }}');
    var longitude = parseFloat('{{ m.rsc[id].computed_location_lng }}');
    if (!isNaN(latitude) && !isNaN(longitude)) {
        map_mark_location(longitude, latitude, 'reset');
    }
    ev.preventDefault();
});

window.map_mark_location_error = function() {
    z_growl_add("{_ Could not find the location. _}");
    $('#location_address').removeClass('disabled');
}

window.map_mark_location = function(longitude, latitude, method) {
    longitude = Math.round(longitude*1000000) / 1000000;
    latitude = Math.round(latitude*1000000) / 1000000;

    var location = [ longitude, latitude ];
    location = ol.proj.transform(location, 'EPSG:4326', 'EPSG:3857');
    if (!map_pin_layer_feature) {
        map_pin_layer_feature = new ol.Feature(new ol.geom.Point(location))
        map.addLayer(
            new ol.layer.Vector ({
              source: new ol.source.Vector ({
                features: [ map_pin_layer_feature ]
              }),
              style: new ol.style.Style ({
                image: new ol.style.Icon({
                  src: '/lib/images/marker.png'
                })
              })
            })
        );
    } else {
        map_pin_layer_feature.setGeometry(new ol.geom.Point(location));
    }
    if (method != 'typing') {
        $('#location_lat').val(latitude.toString());
        $('#location_lng').val(longitude.toString());
    }
    if (method != 'click') {
        map.getView().setCenter(location);
    }
    if (method != 'click' && method != 'typing') {
        $('#location_zoom_level').val("{{ zoom_level }}");
        map.getView().setZoom( parseFloat("{{ zoom_level }}") );
    } else {
        var zoom = Math.round( map.getView().getZoom() );
        $('#location_zoom_level').val( zoom.toString() );
    }
    if (method == 'gps' || method == 'lookup') {
        z_growl_add("{_ Location has been set. _}");
    }
    $('#location_address').removeClass('disabled');
    $('#location_me').removeClass('disabled');
}

{% endjavascript %}
{% endwith %}
{% endwith %}
{% endwith %}
