{# CSS files for maps #}

{% if m.config.mod_geomap.provider.value == 'googlemaps' %}
{% else %}
    {% lib "css/ol.css" %}
{% endif %}
