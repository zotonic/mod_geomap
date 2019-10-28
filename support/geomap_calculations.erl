%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Geo support functions

%% Copyright 2014 Arjan Scherpenisse
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(geomap_calculations).

-export([
         get_lat_lng_bounds/3,
         distance/4
        ]).


%% @doc see http://stackoverflow.com/questions/12424710/php-finding-latitude-and-longitude-boundaries-based-on-a-central-lat-lng-and-di
%% Radius is in kilometers.
get_lat_lng_bounds(Lat, Lng, Radius) ->
    EarthRadius = 6371.009,
    Fraq = Radius / EarthRadius,

    LatMax = Lat + rad2deg(Fraq),
    LatMin = Lat - rad2deg(Fraq),

    %% longitude boundaries (longitude gets smaller when latitude increases)
    LngMax = Lng + rad2deg(Fraq) / math:cos(deg2rad(Lat)),
    LngMin = Lng - rad2deg(Fraq) / math:cos(deg2rad(Lat)),

    {LatMin, LngMin, LatMax, LngMax}.


rad2deg(Rad) -> Rad * 57.29577951308232. %% angle / Math.PI * 180
deg2rad(Deg) -> Deg * 0.01745329251994329.



%% @doc Give distance between 2 points in kilometers (Haversine forumula)
%%  http://www.movable-type.co.uk/scripts/latlong.html
%%  var φ1 = lat1.toRadians(), φ2 = lat2.toRadians(), Δλ = (lon2-lon1).toRadians(), R = 6371; // gives d in km
%%  var d = Math.acos( Math.sin(φ1)*Math.sin(φ2) + Math.cos(φ1)*Math.cos(φ2) * Math.cos(Δλ) ) * R;
distance(Lat1, Lng1, Lat2, Lng2) ->
    L1 = deg2rad(Lat1),
    L2 = deg2rad(Lat2),
    Lambda = deg2rad(Lng2-Lng1),
    R = 6371.0,
    math:acos( math:sin(L1)*math:sin(L2) + math:cos(L1)*math:cos(L2)*math:cos(Lambda) ) * R.

    
