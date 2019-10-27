{% extends "geomap_fullscreen.tpl" %}

{% block content %}
    <div class="row-fluid map" id="map"></div>
    {% lib
        "js/openlayers/ol.js"
        "js/geomap.country.js"
    %}
    {% javascript %}window.GeoMapCountry.init({ attribution: "{{ m.rsc[id].summary }}" });{% endjavascript %}
{% endblock %}

