%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Geo search functions

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

-module(geomap_search).

-export([search_query/2, get_query_center/2]).

-include_lib("zotonic.hrl").

-define(PI, 3.141592653589793).

%% @doc Geo-related searches
search_query(#search_query{ search={geo_nearby, Args} }, Context) ->
    Cats = case proplists:get_all_values(cat, Args) of
        [] -> [];
        Cs -> [{"r", lists:flatten(Cs)}]
    end,
    Distance = z_convert:to_float(proplists:get_value(distance, Args, 10)),
    {Lat, Lng} = get_query_center(Args, Context),
    {LatMin, LngMin, LatMax, LngMax} = geomap_calculations:get_lat_lng_bounds(Lat, Lng, Distance),

    #search_sql{
        select = "r.id",
        from = "rsc r",
        where = "$1 < pivot_location_lat AND $2 < pivot_location_lng AND pivot_location_lat < $3 AND pivot_location_lng < $4",
        cats = Cats,
        order = "(pivot_location_lat-$5)*(pivot_location_lat-$5) + (pivot_location_lng-$6)*(pivot_location_lng-$6)",
        args = [ LatMin, LngMin, LatMax, LngMax, Lat, Lng ],
        tables = [ {rsc,"r"} ]
      };

search_query(#search_query{}, _Context) ->
    undefined. %% fall through


get_query_center(Args, Context) ->
    case {proplists:get_value(lat, Args), proplists:get_value(lng, Args)} of
        {undefined, undefined} ->
            case proplists:get_value(id, Args) of
                undefined ->
                    throw({error, missing_geo_search_parameters});
                Id0 ->
                    Id = m_rsc:name_to_id_check(Id0, Context),
                    case {m_rsc:p(Id, location_lat, Context), m_rsc:p(Id, location_lng, Context)} of
                        {Lat, Lng} when is_float(Lat), is_float(Lng) ->
                            {Lat, Lng};
                        _ ->
                            throw({error, {rsc_without_location, Id}})
                    end
            end;
        {Lat, Lng} ->
            {z_convert:to_float(Lat),
             z_convert:to_float(Lng)}
    end.
