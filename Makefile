PROJECT = csvlog

ERL = erl
REBAR = ./rebar

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

test:
	@$(REBAR) xref ct

run: compile
	$(ERL) -pa ebin -config $(PROJECT) -s $(PROJECT)
