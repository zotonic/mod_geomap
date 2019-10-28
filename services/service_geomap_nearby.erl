%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Return the JSON for the country overview.

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

-module(service_geomap_nearby).

-export([process_get/2]).

-include_lib("zotonic.hrl").

process_get(_, Context) ->
    QArgs = lists:foldl(
        fun(P, Acc) ->
            case z_context:get_q(P, Context) of
                undefined -> Acc;
                "" -> Acc;
                <<>> -> Acc;
                V -> [ {P, V} | Acc ]
            end
         end,
         [],
         [id, cat, distance, lat, lng]),
    #search_result{ result = Ids } = z_search:search({geo_nearby, QArgs}, Context),
    {Lat, Lng} = geomap_search:get_query_center(QArgs, Context),
    {array, lists:sort(map_results(Ids, Lat, Lng, Context))}.


map_results(Ids, Lat, Lng, Context) ->
    lists:map(
        fun(Id) ->
            Props = export(Id, Context),
            D = geomap_calculations:distance(Lat, Lng, proplists:get_value(location_lat, Props), proplists:get_value(location_lng, Props)),
            z_convert:to_json([{distance, D} | Props])
        end,
        Ids).

export(Id, Context) ->
    ImageUrl = case z_media_tag:url(m_rsc:p(Id, depiction, Context), [{width, 400}], Context) of
       {ok, U} -> [{image_url, U}];
       _ -> []
    end,
    lists:map(
        fun(K) ->
            {K, m_rsc:p(Id, K, Context)}
        end,
        [id, title, summary, location_lat, location_lng, location_zoom_level, created, modified, publication_start])
    ++ ImageUrl.

