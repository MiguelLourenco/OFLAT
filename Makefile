APP = OFLAT
VERSION = 2.1
COMP = ocamlc
SRC_DIR = src
OBJ_DIR = obj
APP_DIR = $(APP)
FLAGS = -w -8 -g
PACKAGES = js_of_ocaml,js_of_ocaml-ppx,js_of_ocaml-lwt,js_of_ocaml-tyxml
OCAMLFLAT = OCamlFlat
OCAMLFLAT_BASE = ../$(OCAMLFLAT)
OCAMLFLAT_LIB = $(OCAMLFLAT_BASE)/lib/$(OCAMLFLAT).ml
NAMES = OCamlFlat Lang JS Graphics Listeners Cytoscape					\
		FiniteAutomatonGraphics RegularExpressionGraphics				\
		ContextFreeGrammarBasicGraphics ContextFreeGrammarLL1Graphics	\
		ContextFreeGrammarLRGraphics ContextFreeGrammarGraphics			\
		TuringMachineGraphics StateVariables HtmlPageClient				\
		Controller Calling Start			


define OBJS
	$(addprefix $(OBJ_DIR)/, $(addsuffix .cmo, $(foreach file, $(NAMES), $(file))))
endef

define SRCS
	$(addprefix $(SRC_DIR)/, $(addsuffix .ml, $(foreach file, $(NAMES), $(file))))
endef

OS = $(shell uname -s)
ifeq ($(OS),Linux)
	OPEN_BROWSER = "open"
else
	OPEN_BROWSER = "open"
endif

$(APP_DIR)/$(APP).js: $(OBJ_DIR)/$(APP).byte
	@echo "BUILDING" $@
	@js_of_ocaml --pretty --no-inline --debug-info --source-map $(APP_DIR)/GraphLibrary.js $< -o $@

$(OBJ_DIR)/$(APP).byte: $(OBJS)
	@echo "LINKING" $@
	@ocamlfind $(COMP) $(FLAGS) -package $(PACKAGES) -linkpkg -o $@ $(OBJS)

$(OBJS): $(SRCS)
	@echo "COMPILING" $(SRC_DIR)/\*.ml
	@ocamlfind $(COMP) $(FLAGS) -I $(SRC_DIR) -package $(PACKAGES) -c $(SRCS) || rm -f $(SRC_DIR)/*.cm[io]
	@mv $(SRC_DIR)/*.cm[io] $(OBJ_DIR)

#$(OBJ_DIR)/%.cmo $(OBJ_DIR)/%.cmi: src/%.ml
#	@echo "COMPILING" $<
#	@ocamlfind $(COMP) $(FLAGS) -I $(OBJ_DIR) -package $(PACKAGES) -c $<
#	@mv $(SRC_DIR)/*.cm[io] $(OBJ_DIR)

$(SRC_DIR)/$(OCAMLFLAT).ml:
	@echo "BUILDLING" $@
	@cd $(OCAMLFLAT_BASE) && $(MAKE)
	@echo "INSTALLING" $@
	@cp $(OCAMLFLAT_LIB) $@

$(OBJS): | $(OBJ_DIR)

$(OBJ_DIR):
	@mkdir -p $(OBJ_DIR)

.PHONY: run
run: $(APP_DIR)/$(APP).js
	@$(OPEN_BROWSER) $(APP_DIR)/index.html > /dev/null 
	@echo "OPENED $(APP) in the browser"

.PHONY: runl
runl: $(APP_DIR)/$(APP).js
	@$(OPEN_BROWSER) "file://""`pwd`""/$(APP_DIR)/index.html?logging2" > /dev/null &
	@echo "OPENED $(APP) in the browser (using active logging)"

.PHONY: edit
edit:
	@cd $(SRC_DIR) ; geany ../$(APP).geany &

.PHONY: editreset
editreset:
	cp $(OCAMLFLAT_BASE)/geany.template $(APP).geany
	sed -i -e "s/PROJECT_NAME/$(APP)/" $(APP).geany
	@echo PLEASE, activate the plugins "File Browser" and "Split Window"; sleep 3
	$(MAKE) edit

.PHONY: clean
clean:
	rm -rf $(APP_DIR)/$(APP).* obj

.PHONY: cleanfull
cleanfull: clean
	rm -f $(SRC_DIR)/$(OCAMLFLAT).ml

.PHONY: update
update: $(APP_DIR)/$(APP).js
	cp -a $(APP)/* ~/www/leafs/$(APP)

.PHONY: git0
git0: clean
	git fetch
	git status

.PHONY: git1
git1: clean
	git fetch
	git diff

.PHONY: git2
git2: clean
	git fetch
	git add src/*.ml $(APP)/* Makefile
	git status

.PHONY: git3
git3: clean
	git fetch
	git commit
	git status

.PHONY: git4
git4:
	git push origin master

.PHONY: gitp
gitp:
	git pull

