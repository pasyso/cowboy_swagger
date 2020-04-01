%%% @doc cowboy-swagger main interface.
-module(cowboy_swagger).

%% API
-export([to_json/1, add_definition/2, add_definition_array/2, schema/1]).

%% Utilities
-export([enc_json/1, dec_json/1]).
-export([swagger_paths/1, validate_metadata/1]).
-export([filter_cowboy_swagger_handler/1]).
-export([get_existing_definitions/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-opaque parameter_obj() ::
  #{ name        => iodata()
   , in          => iodata()
   , description => iodata()
   , required    => boolean()
   , type        => iodata()
   , schema      => iodata()
   }.
-export_type([parameter_obj/0]).

-opaque response_obj() ::
  #{ description => binary()
   }.
-type responses_definitions() :: #{binary() => response_obj()}.
-export_type([response_obj/0, responses_definitions/0]).

-type parameter_definition_name () :: binary().
-type property_desc() ::
  #{ type => binary()
   , description => binary()
   , example => binary()
   , items => property_desc()
   }.
-type property_obj() :: #{binary() => property_desc()}.
-type parameters_definitions() ::
  #{parameter_definition_name() =>
      #{ type => binary()
       , properties => property_obj()
       }}.
-type parameters_definition_array() ::
  #{parameter_definition_name() =>
      #{ type => binary()
       , items => #{ type => binary()
                   , properties => property_obj()
                   }
       }}.
-export_type([ parameter_definition_name/0
             , property_obj/0
             , parameters_definitions/0
             , parameters_definition_array/0
             ]).

%% Swagger map spec
-opaque swagger_map() ::
  #{ description => iodata()
   , summary     => iodata()
   , parameters  => [parameter_obj()]
   , tags        => [iodata()]
   , consumes    => [iodata()]
   , produces    => [iodata()]
   , responses   => responses_definitions()
   }.
-type metadata() :: trails:metadata(swagger_map()).
-export_type([swagger_map/0, metadata/0]).

-type swagger_version() :: swagger_2_0
                         | openapi_3_0_0.
-export_type([swagger_version/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the swagger json specification from given `trails'.
%%      This function basically takes the metadata from each `trails:trail()'
%%      (which must be compliant with Swagger specification) and builds the
%%      required `swagger.json'.
-spec to_json([trails:trail()]) -> jsx:json_text().
to_json(Trails) ->
  Default = #{info => #{title => <<"API-DOCS">>}},
  GlobalSpec = normalize_map_values(
    application:get_env(cowboy_swagger, global_spec, Default)),
  SanitizeTrails = filter_cowboy_swagger_handler(Trails),
  SwaggerSpec = create_swagger_spec(GlobalSpec, SanitizeTrails),
  enc_json(SwaggerSpec).

-spec add_definition_array( Name::parameter_definition_name()
                          , Properties::property_obj()
                          ) ->
  ok.
add_definition_array(Name, Properties) ->
  DefinitionArray = build_definition_array(Name, Properties),
  add_definition(DefinitionArray).

-spec add_definition( Name::parameter_definition_name()
                    , Properties::property_obj()
                    ) ->
  ok.
add_definition(Name, Properties) ->
  Definition = build_definition(Name, Properties),
  add_definition(Definition).

-spec add_definition( Definition :: parameters_definitions()
                                  | parameters_definition_array()
                    ) ->
  ok.
add_definition(Definition) ->
  CurrentSpec = application:get_env(cowboy_swagger, global_spec, #{}),
  NewDefinitions = maps:merge( get_existing_definitions(CurrentSpec)
                             , Definition
                             ),
  NewSpec = prepare_new_global_spec(CurrentSpec, NewDefinitions),
  application:set_env(cowboy_swagger, global_spec, NewSpec).

-spec schema(DefinitionName::parameter_definition_name()) ->
  map().
schema(DefinitionName) ->
  case swagger_version() of
    swagger_2_0 ->
      #{<<"$ref">> => <<"#/definitions/", DefinitionName/binary>>};
    openapi_3_0_0 ->
      #{<<"$ref">> => <<"#/components/schemas/", DefinitionName/binary>>}
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec enc_json(jsx:json_term()) -> jsx:json_text().
enc_json(Json) ->
  jsx:encode(Json, [uescape]).

%% @hidden
-spec dec_json(iodata()) -> jsx:json_term().
dec_json(Data) ->
  try jsx:decode(Data, [return_maps])
  catch
    _:{error, _} ->
      throw(bad_json)
  end.

%% @hidden
-spec swagger_paths([trails:trail()]) -> map().
swagger_paths(Trails) ->
  swagger_paths(Trails, #{}).

%% @hidden
-spec validate_metadata(trails:metadata(_)) -> metadata().
validate_metadata(Metadata) ->
  validate_swagger_map(Metadata).

%% @hidden
-spec filter_cowboy_swagger_handler([trails:trail()]) -> [trails:trail()].
filter_cowboy_swagger_handler(Trails) ->
  F = fun(Trail) ->
    MD = trails:metadata(Trail),
    maps:size(maps:filter(fun is_visible/2, MD)) /= 0
  end,
  lists:filter(F, Trails).

-spec get_existing_definitions(CurrentSpec :: map()) ->
  Definition :: parameters_definitions()
              | parameters_definition_array().
get_existing_definitions(CurrentSpec) ->
  case swagger_version() of
    swagger_2_0 ->
      maps:get(definitions, CurrentSpec, #{});
    openapi_3_0_0 ->
      case CurrentSpec of
        #{components :=
            #{schemas := Schemas }} -> Schemas;
        _Other                      -> #{}
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
-spec swagger_version() -> swagger_version().
swagger_version() ->
  case application:get_env(cowboy_swagger, global_spec, #{}) of
    #{openapi := "3.0.0"} -> openapi_3_0_0;
    #{swagger := "2.0"}   -> swagger_2_0;
    _Other                -> swagger_2_0
  end.

%% @private
is_visible(_Method, Metadata) ->
  not maps:get(hidden, Metadata, false).

%% @private
swagger_paths([], Acc) ->
  Acc;
swagger_paths([Trail | T], Acc) ->
    Metadata = validate_metadata(trails:metadata(Trail)),
    Metadata11 = normalize_map_values(Metadata),
    Path_list = cowboy_path_variants(trails:path_match(Trail), []),
    NewAcc = lists:foldl(
        fun (PathVar, VarAcc) ->
            Metadata1 = validate_swagger_map(Metadata11, [{path_params, extract_path_params(PathVar)}]),
            %%Metadata2 = normalize_map_values(Metadata1),
            PathVar2 = normalize_path(PathVar),
            maps:put(PathVar2, Metadata1, VarAcc)
        end, Acc, Path_list),
    swagger_paths(T, NewAcc).

%% @private
cowboy_path_variants(Path, undefined) -> cowboy_path_variants(Path, []);
cowboy_path_variants(Path, Acc) ->
    case re:run(Path, "\\[([^\\[|\\]]+)\\]", [{capture, first}]) of
        nomatch -> [Path|Acc];
        {match, [{Pos, Len}]} ->
            P_start = binary:part(Path, 0, Pos),
            P_middle = binary:part(Path, Pos+1, Len-2),
            P_end = binary:part(Path, Pos + Len, size(Path) - Pos - Len),
            Acc1 = cowboy_path_variants(<<P_start/binary, P_end/binary>>, Acc),
            cowboy_path_variants(<<P_start/binary, P_middle/binary, P_end/binary>>, Acc1)
    end.

extract_path_params(Path) ->
    case re:run(Path, "\\:(\\w+)", [global, {capture, [1], binary}]) of
        {match, Path_params} -> lists:flatten(Path_params);
        nomatch -> []
    end.

%% @private
normalize_path(Path) ->
  re:replace(
    re:replace(Path, "\\:\\w+", "\\{&\\}", [global]),
    "\\[|\\]|\\:", "", [{return, binary}, global]).

%% @private
normalize_map_values(Map) when is_map(Map) ->
  normalize_map_values(maps:to_list(Map));
normalize_map_values(Proplist) ->
  F = fun({K, []}, Acc) ->
        maps:put(K, normalize_list_values([]), Acc);
      ({K, V}, Acc) when is_list(V) ->
        case io_lib:printable_list(V) of
          true  -> maps:put(K, list_to_binary(V), Acc);
          false -> maps:put(K, normalize_list_values(V), Acc)
        end;
      ({K, V}, Acc) when is_map(V) ->
        maps:put(K, normalize_map_values(V), Acc);
      ({K, V}, Acc) ->
        maps:put(K, V, Acc)
      end,
  lists:foldl(F, #{}, Proplist).

%% @private
normalize_list_values(List) ->
  F = fun(V, Acc) when is_list(V) ->
          case io_lib:printable_list(V) of
            true  -> [list_to_binary(V) | Acc];
            false -> [normalize_list_values(V) | Acc]
          end;
      (V, Acc) when is_map(V) ->
        [normalize_map_values(V) | Acc];
      (V, Acc) ->
        [V | Acc]
      end,
  lists:foldr(F, [], List).

%% @private
create_swagger_spec(#{swagger := _Version} = GlobalSpec, SanitizeTrails) ->
    GlobalSpec#{paths => swagger_paths(SanitizeTrails)};
create_swagger_spec(#{openapi := _Version} = GlobalSpec, SanitizeTrails) ->
  GlobalSpec#{paths => swagger_paths(SanitizeTrails)};
create_swagger_spec(GlobalSpec, SanitizeTrails) ->
  create_swagger_spec(GlobalSpec#{openapi => <<"3.0.0">>}, SanitizeTrails).

%% @private
validate_swagger_map(Map) ->
    validate_swagger_map(Map, []).

%% @private
validate_swagger_map(Map, Options) ->
    F = fun(_K, V) ->
        Params = validate_swagger_map_params(maps:get(parameters, V, []), Options),
        Responses = validate_swagger_map_responses(maps:get(responses, V, #{})),
        V#{parameters => Params, responses => Responses}
        end,
    maps:map(F, Map).

%% @private
validate_swagger_map_params(Params, Options) ->
    ValidateParams =
        fun(E) ->
            case maps:get(name, E, undefined) of
                undefined -> false;
                _         -> case maps:get(in, E, <<"path">>) of
                                 path -> {true, E#{required => true}};
                                 <<"path">> -> {true, E#{in => path, required => true}};
                                 _ -> {true, E}
                             end
            end
        end,
    Params1 = lists:filtermap(ValidateParams, Params),
    case proplists:get_value(path_params, Options) of
        undefined -> Params1;
        Tp ->
            {True_rest, RevParams} = lists:foldl(
                fun (E = #{in := path, name := Name}, {True_plist, Acc_p}) ->
                    case lists:member(Name, True_plist) of
                        true -> {lists:delete(Name, True_plist), [E|Acc_p]};
                        _    -> {True_plist, Acc_p}
                    end;
                    (E, {True_plist, Acc_p}) -> {True_plist, [E|Acc_p]}
                end, {Tp, []}, Params1),
            Params2 = lists:reverse(RevParams)
                ++ [#{name => P_name, in => path, required => true, type => string} || P_name <- True_rest],
            lists:sort(
                fun (#{in := X}, #{in := X}) -> true;
                    (#{in := _}, #{in := header}) -> false;
                    (#{in := header}, #{in := path}) -> true;
                    (#{in := _}, #{in := path}) -> false;
                    (_, _) -> true
                end, Params2)
    end.

%% @private
validate_swagger_map_responses(Responses) ->
  F = fun(_K, V) -> V#{description => maps:get(description, V, <<"">>)} end,
  maps:map(F, Responses).

%% @private
-spec build_definition( Name::parameter_definition_name()
                      , Properties::property_obj()
                      ) ->
  parameters_definitions().
build_definition(Name, Properties) ->
  #{Name => #{ type => <<"object">>
             , properties => Properties
             }}.

%% @private
-spec build_definition_array( Name::parameter_definition_name()
                            , Properties::property_obj()
                            ) ->
  parameters_definition_array().
build_definition_array(Name, Properties) ->
  #{Name => #{ type => <<"array">>
             , items => #{ type => <<"object">>
                         , properties => Properties
                         }
             }}.

%% @private
-spec prepare_new_global_spec( CurrentSpec :: map()
                             , Definitions :: parameters_definitions()
                                            | parameters_definition_array()
                             ) ->
  NewSpec :: map().
prepare_new_global_spec(CurrentSpec, Definitions) ->
  case swagger_version() of
    swagger_2_0 ->
      CurrentSpec#{definitions => Definitions
                  };
    openapi_3_0_0 ->
      CurrentSpec#{components =>
                    #{ schemas => Definitions
                     }
                  }
  end.