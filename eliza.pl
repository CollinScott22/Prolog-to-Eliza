% The simplification database that allows the user to type more than one
% thing and get more intelligent responses by replacing the initially
% typed word with a word that fits a rule in the reply database
sr([do,not|X],[dont|Y],X,Y).
sr([can,not|X],[cant|Y],X,Y).
sr([cannot|X],[cant|Y],X,Y).
sr([will,not|X],[wont|Y],X,Y).
sr([dreamed|X],[dreamt|Y],X,Y).
sr([dreams|X],[dream|Y],X,Y).
sr([how|X],[what|Y],X,Y).
sr([when|X],[what|Y],X,Y).
sr([alike|X],[dit|Y],X,Y).
sr([same|X],[dit|Y],X,Y).
sr([certainly|X],[yes|Y],X,Y).
sr([maybe|X],[perhaps|Y],X,Y).
sr([norse|X],[xfrem|Y],X,Y).
sr([francais|X],[xfrem|Y],X,Y).
sr([espanol|X],[xfrem|Y],X,Y).
sr([machine|X],[computer|Y],X,Y).
sr([machines|X],[computer|Y],X,Y).
sr([computers|X],[computer|Y],X,Y).
sr([im|X],[youre|Y],X,Y).
sr([youre|X],[im|Y],X,Y).
sr([am|X],[are|Y],X,Y).
sr([your|X],[my|Y],X,Y).
sr([were|X],[was|Y],X,Y).
sr([me|X],[you|Y],X,Y).
sr([you,are|X],[im|Y],X,Y).      % im = i'm = i am
sr([i,am|X],[youre|Y],X,Y).      % youre = you're = you are =\= your
sr([myself|X],[yourself|Y],X,Y).
sr([yourself|X],[myself|Y],X,Y).
sr([mom|X],[mother|Y],X,Y).
sr([dad|X],[father|Y],X,Y).
sr([i|X],[you|Y],X,Y).
sr([you|X],[me|Y],X,Y).
sr([my|X],[your|Y],X,Y).
sr([everybody|X],[everyone|Y],X,Y).
sr([nobody|X],[noone|Y],X,Y).
% new rules
sr([you,remind,me,of|X],[dit|Y],X,Y).
sr([like|X],[dit|Y],X,Y).
sr([bye|X],[quit|Y],X,Y).
sr([goodbye|X],[quit|Y],X,Y).
sr([hi|X],[hello|Y],X,Y).
sr([ok|X],[yes|Y],X,Y).
sr([okay|X],[yes|Y],X,Y).
sr([yup|X],[yes|Y],X,Y).

% The rules database gives a reply based on what keyword is given along
% with score
rules(1, 0,[sorry|_],[
		[please,do,not,apologize,.],
		[apologies,are,not,necessary,.],
		[what,feelings,do,you,have,when,you,apologize,?],
		['I',have,told,you,that,apologies,are,not,required,.]]).

rules(2,0,[i,feel|_],[why,do,you,think,that,is,?]).
rules(3,0,[alike|_],[what,is,the,connection,do,you,suppose,?]).
rules(4,0,[final,project|_],[how,did,you,feel,about,this,project,?]).
rules(5,0,[dream|_],[
	[what,does,that,dream,suggest,to,you,?],
	[do,you,dream,often,?],
	[what,was,special,about,about,your,dream,?]]).
rules(6,0,[hungry|_],[what,do,you,want,to,eat,?]).
rules(7,0,[spaghetti|_],[oh,!,that,sounds,like,a,good,choice,for,lunch]).
rules(8,0,[_],[
	[please,tell,me,more,...],
	[what,does,that,suggest,to,you,?],
	[please,go,on,.],
	[do,you,feel,strongly,about,such,things]]).
rules(9,0,[perhaps|_],[
	[you,dont,seem,certain,...],
	[why,so,uncertain,?],
	[are,you,not,sure,?],
	[do,you,not,know,?]]).

rules(10,0,[stressed|_],[i,am,sorry,that,you,feel,stressed]).

rules(11,0,[i,feel|Y],[
	[how,long,have,you,felt,Y,?],
	[when,did,you,start,feeling,Y,?],
	[can,you,explain,what,made,you,Y,?]]).

rules(12,0,[hello|_],[
	       [how,do,you,do,'.',please,state,your,problem,.]]).
rules(13,5,[computer|_],[
	       [do,computers,worry,you,?]]).

rules(14,7,[xfrem|_],[[sorry,i,only,speak,english]]).
rules(15,-1,[_],[[please,tell,me,more,...]]).
rules(16,0,[are,you|Y],[[would,you,want,to,be,Y,?],
		       [do,you,believe,you,are,Y,?]]).
rules(17,0,[are|_],[[i,do,not,understand,...]]).
rules(18,0,[are|Y],[[what,if,they,were,not,Y]]).
rules(19,0,[my|Y],[[why,are,you,concerned,about,my,Y]]).
rules(20,10,[dit|_],[[in,what,way,?]]).
rules(21, 100,[quit|_],[
		[goodbye,.,'My',secretary,will,send,you,a,bill,.]]).
rules(22,0,[your|Y],[[your,Y,?]]).
rules(23,0,[yes|_],[[you,seem,very,positive]]).
rules(24,0,[i|_],[[what,are,your,feelings,now,?]]).
rules(25,0,[im|Y],[[what,makes,you,think,'I',am,Y]]).
rules(26,1,[you,feel,Y],[[do,you,often,feel,Y,?]]).
rules(27,1,[was,you|Y],[[what,if,you,were,Y,?]]).
rules(28,1,[youre|Res],[['I',am,sorry,to,hear,you,are,M],
			[can,you,explain,what,made,you,M]]):- member(M,Res),
			member(M,[sad,depressed,stressed,sick,lost]).

rules(29,1,[youre|Y],[[did,you,come,to,me,because,of,Y,?]]).
rules(30,0,[i,Y],[[we,were,discussing,you,',',not,me,.],
		 [oh,',',i,Y,huh,what,makes,you,think,that,?]]).

rules(ID, Score,[im|X],R):-rules(ID, Score,[i|X],R),!.
rules(ID, Score,[youre|X],R):-rules(ID, Score,[you|X],R),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% I/O interface %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% out(+Text):- prints output to some output stream
out(Text):-write(Text).

% in(+Text):- reads question from input stream
in(Text):-
    %nl creates a new line
	nl,
    %write("> ") creates a a symbol > in the input line to simulate a chat box
	write("> "),
    %readLine(Text) reads the actual input
	readLine(Text).

readLine(Text):-
    %getchar(Char) pops the first char out of the input and records it
	get_char(Char),
    %toLowerCase(Char,LChar) changes the character to lowercase
	toLowerCase(Char,LChar),
    %readLine2(LChar,Text) reads in the next character and continues the loop with an exit case
	readLine2(LChar,Text).
readLine2('\n',[]):-!.
readLine2(LChar,[LChar|T]):-readLine(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Basic utilities  %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% constant predicates
charType('!', punctuation).
charType('?', punctuation).
charType('.', punctuation).
charType(',', punctuation).
charType('\'', punctuation).
charType(' ', whitespace).
charType('\t', whitespace).

% toLowerCase(+Char, -LChar):- lower case char (using ASCI codes)
toLowerCase(Char, LChar):-
	char_code(Char, Code),
	Code >= "A",
	Code =< "Z",
	NewCode is Code + 32,
	char_code(LChar, NewCode), !.
toLowerCase(Char, Char).

% toUpperCase(+Char, -UChar):- upper case char (using ASCI codes)
toUpperCase(Char, UChar):-
	char_code(Char, Code),
	Code >= "a",
	Code =< "z",
	NewCode is Code - 32,
	char_code(UChar, NewCode), !.
toUpperCase(Char, Char).

% deleteChars(+Line, -Type, -Res):- delete specific charType from line
deleteChars([Char|Rest],Type,Out):-
	charType(Char, Type),
	deleteChars(Rest,Type,Out),!.

deleteChars([Char|Rest],Type,[Char|Out]):-
	deleteChars(Rest,Type,Out),!.

deleteChars([],_,[]).

% toWords(+Line, -Words):- transfer output of readLine to list of words
toWords([],[]):-!.
toWords(Line, [Word|ResWords]):-
	readWord(Line, Word, ResLine),
	toWords(ResLine, ResWords).

% readWord(+Line, -Word, -ResLine) :- reads one word from line
%	(the rest of line is returned in ResLine
readWord([], '', []).
readWord([Char|Res], '', Res) :- charType(Char, whitespace),!.
readWord([Char|ResLine], Word, Res) :-
	readWord(ResLine, ResWord, Res),
	atom_concat(Char, ResWord, Word).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%  Eliza function  %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SWI-Prolog was updated to a new version of Prolog that allowed me to
% declare any predicate as dynamic, not just the built-in ones.
:- dynamic res/2.
res(_,0).

% simplify(+In,-Out):- removes unnecessary characters eg. "," and "."
%	and simplify words
simplify(In, Out):-
	deleteChars(In, punctuation, Out1),
	toWords(Out1,Out2),
	findSynonyms(Out2,Out3),
	Out = Out3.

 %findSynonyms(+Words, -Synonyms) :- finds synonyms using
%	simplification rules (loaded by init function)
findSynonyms(Words, Syn) :-
	sr(Words, Syn, RestWords, ResOutput),!,
	findSynonyms(RestWords, ResOutput).
findSynonyms([Word| ResWords], [Word| ResSyn]):-
	findSynonyms(ResWords, ResSyn),!.
findSynonyms([], []).

% findReply(+Words, -Reply) :- finds reply with highest rank
%	(loaded by init function)
findReply(Words, Reply) :-
	findReply2(Words, -2, 0, [], ID, Reply),
	ID \= 0,
	updateRes(ID).

% findReply2(+Words, +ActScore, +ActRuleID, +ActRes, -RuleID, -Res):- finds reply using two
%	accumulators
findReply2([H|T], ActScore, _, _, ID, Res):-
	findall(Score,rules(_, Score,[H|T],_),Rules),
	Rules \= [], % bagof doesn't work as I except
	max_list(Rules,NewScore),
	ActScore < NewScore,
	rules(NewID, NewScore,[H|T],Replyes),
	res(NewID,ResID),
	nth0(ResID,Replyes,NewReply),
	findReply2(T, NewScore, NewID, NewReply, ID, Res),!.
findReply2([_|T], ActScore, ActID, ActRes, ID, Res):-
	findReply2(T, ActScore, ActID, ActRes, ID, Res).
findReply2([], _, ID, Res, ID, Res).

% updateResID(+ID):- moves to next reply for rule
updateRes(ID):-
	res(ID,RID),
	once(rules(ID,_,_,Replyes)),
	length(Replyes, Len),
	NRID is (RID + 1) mod Len,
	retract((resID(ID,RID):-!)),
	asserta(resID(ID,NRID):-!),!.
updateRes(ID):-
	res(ID,RID),
	once(rules(ID,_,_,Replyes)),
	length(Replyes, Len),
	NRID is (RID + 1) mod Len,
	asserta(resID(ID,NRID):-!).

% writeWords(+Words) - uppers first letter and writes words to output
writeWords([Word|Res]):-
	string_chars(Word,[Char|RChar]),
	toUpperCase(Char,UChar),
	readWord([UChar|RChar],Out,_),
	out(Out),
	writeWords2(Res).
% writes inner list
writeWords2([Word|Res]):-
	is_list(Word),
	writeWords2(Word),
	writeWords2(Res),!.
% writes punctuation
writeWords2([Word|Res]):-
	charType(Word,punctuation),
	out(Word),
	writeWords2(Res),!.
% writes standard char
writeWords2([Word|Res]):-
	out(" "),
	out(Word),
	writeWords2(Res),!.
writeWords2([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%  Main function  %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eliza:-
	out("Looking for Eliza....\n"),
	out("Here she is...\n\n"),
	out("Hello, I\'m Eliza, how can I help you?"),
	eliza([hi]).

eliza([quit|_]):-!.
eliza(_):-
	in(Line),
	simplify(Line, Words),
	findReply(Words,Reply),
	writeWords(Reply),nl,
	eliza(Words).














