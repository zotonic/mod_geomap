{% extends "base_fullscreen.tpl" %}

{% block title %}{% if q.id %}{_ Map of _} {{ m.rsc[q.id].title }}{% else %}{_ Map _}{% endif %}{% endblock %}

{% block html_head_extra %}
    {% lib "css/ol.css" %}
    <style type="text/css">
        div#map {
            height: 100%;
            width: 100%;
            padding: 0;
        }
        div.olControlZoom {
            top: 90px;
            left: 16px;
        }
    </style>
{% endblock %}

{% block content %}
    <div class="row-fluid map do_geomap_country" id="map"></div>
    {% lib
        "js/openlayers/z.geomap_country.js"
    %}
    {% comment %}
        {% javascript %}window.GeoMap.init({ attribution: "{{ m.rsc[id].summary }}" });{% endjavascript %}
    {% endcomment %}
{% endblock %}


