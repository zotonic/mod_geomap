mod_geomap
==========

Support for displaying maps and adding geographical locations to
:term:`resources <resource>` in the admin.

Calculates quadtile codes from lat/long location stored in Zotonic
resources, and other geo-related functionality.

Uses OpenStreetMap for display of maps in the admin and on Zotonic
websites.


Custom tag: geomap_static
-------------------------

Shows a location’s map using static images from OpenStreetMap.

    {% geomap_static latitude=52.34322 longitude=4.33423 %}

The location is taken from the tag’s `latitude` + `longitude`
parameters, or, when absent, from the `id` parameter which is supposed
to be a Zotonic resource of category `location` (e.g. having an
address).

It displays the `_geomap_static.tpl` template.

Other parameters:

`zoom`
  Zoom level (defaults to 14)

`n`
  How many rows and cols to display (defaults for the `rows` and `cols` parameters), defaults to 2.

`cols`
  How many grid columns to display

`rows`
  How many grid rows to display

`size`
  The size in pixels of each tile, defaults to 256.



Countries API
-------------

The module exposes an API service at `/api/geomap/countries`, which
returns an array of countries as installed by :ref:`mod_geomap`, with
geographical coordinates and outline area.

Every country has the following JSON object returned:
  
    {'geometry': {'coordinates': [[[[35.26, 31.79],
                                                 [35.25, 31.79],
                                                 [35.25, 31.81],
                                                 [35.26, 31.79]]],
                                               [[[35.62, 33.25],
                                                 [35.65, 32.69],
                                                 [35.55, 32.39],
                                                 [35.28, 32.52],
                                                 [34.88, 31.39],
                                                 [35.48, 31.5],
                                                 [34.98, 29.55],
                                                 [34.9, 29.49],
                                                 [34.27, 31.22],
                                                 [34.33, 31.26],
                                                 [34.49, 31.6],
                                                 [35.1, 33.09],
                                                 [35.62, 33.25]]]],
                              'type': 'MultiPolygon'},
                'id': 376,
                'properties': {'colour': '#ccc',
                                'name': 'Israel',
                                'value': ''},
                'type': 'Feature'},


Locations API
-------------

The module exposes an API service at `/api/geomap/locations`, which
returns a list of locations for the search or id given.
