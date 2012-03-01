%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Convert a proplist into a record.
%%%   ?PROPS_TO_RECORD(PropList, RecordSymbol) -> Record 
%%%
%%% Created : Feb 26, 2012
%%% -------------------------------------------------------------------
-author("Sungjin Park <jinni.park@gmail.com>").

-define(PROPS_TO_RECORD(Props, Record), ?PROPS_TO_RECORD(Props, Record, #Record{})()).
-define(PROPS_TO_RECORD(Props, Record, Default),
		fun() ->
			Fields = record_info(fields, Record),
			[Record | Defaults] = tuple_to_list(Default),
			List = [proplists:get_value(F, Props, D) || {F, D} <- lists:zip(Fields, Defaults)],
			list_to_tuple([Record | List])
		end).
