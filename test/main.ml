open OUnit2
open Enigma

(** [index_test name input expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [index input]. *)
let index_test (name : string) (input : char) (expected_output : int) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (index input) ~printer:string_of_int

(* You will find it helpful to write functions like [index_test] for each of the
   other functions you are testing. They will keep your lists of tests below
   very readable, and will also help you to avoid repeating code. You will also
   find it helpful to create [~printer] functions for the data types in use. *)

let index_tests =
  [ index_test "index of A is 0" 'A' 0 ;
  index_test "index of C is 2" 'C' 2 ;
  index_test "index of B is 1" 'B' 1 ;
  index_test "index of D is 3" 'D' 3 ;
  index_test "index of Z is 25" 'Z' 25 ;
  ]
let map_rl_test (name: string)(wiring : string) (top_letter : char) (input_pos : int) (output_pos : int): test = 
  name >:: fun _ ->
  assert_equal output_pos (map_r_to_l wiring top_letter input_pos) ~printer:(string_of_int : int -> string)
let map_rl_tests = [ map_rl_test "test1" "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'A' 0 0;
map_rl_test "test2" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'A' 0 4;
map_rl_test "test3" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'B' 0 9; 
map_rl_test "test4" "BDFHJLCPRTXVZNYEIWGAKMUSQO" 'A' 14 24;
map_rl_test "test5" "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'A' 1 0 ;
map_rl_test "test6" "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'B' 2 2;
map_rl_test "test7" "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'C' 3 3;]
let map_lr_test (name: string)(wiring : string) (top_letter : char) (input_pos : int) (output_pos : int): test = 
  name >:: fun _ ->
  assert_equal output_pos (map_l_to_r wiring top_letter input_pos) ~printer:(string_of_int : int -> string)
let map_lr_tests = [  map_lr_test "test1" "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'A' 0 0;
map_lr_test "test2" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'F' 10 14;
map_lr_test "test4" "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'A' 1 0;
map_lr_test "test5" "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'B' 2 2;
map_lr_test "test6" "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'C' 3 3;

 ]

let map_refl_test (name: string) (wiring : string) (input_pos : int) (output_pos : int): test =
  name >:: fun _ ->
    assert_equal output_pos (map_refl wiring  input_pos) ~printer:(string_of_int : int -> string)

let map_refl_tests = [ map_refl_test "test1" "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 3 3;
map_refl_test "test2" "YRUHQSLDPXNGOKMIEBFZCWVJAT" 3 7;
map_refl_test "test3" "FVPJIAOYEDRZXWGCTKUQSBNMHL" 5 0;
map_refl_test "test4" "BACDEFGHIJKLMNOPQRSTUVWXYZ" 5 5;
map_refl_test "test5" "FVPJIAOYEDRZXWGCTKUQSBNMHL" 7 24 ;]

let map_plug_test (name : string)(plugs : (char * char) list) (c : char)(output : char): test =
  name >:: fun _ ->
  assert_equal output (map_plug plugs  c)  ~printer:(String.make 1 )
let map_plug_tests = [map_plug_test "0 cable" [] 'a' 'a'; 
map_plug_test "a and b" [('a','b')] 'a' 'b';
map_plug_test "c and d" [('a','b'); ('c','d')] 'c' 'd' ;    
map_plug_test "d and d" [('a','c'); ('d','d')] 'd' 'd' ; 
map_plug_test "d and d" [('e',' '); ('d','d')] 'e' ' ' ; ]  
  
let cipher_char_test (name : string)(config : config) (c : char)(output : char): test =
  name >:: fun _ ->
  assert_equal output (cipher_char config c)  ~printer:(Char.escaped )

let rotor1: rotor ={wiring="EKMFLGDQVZNTOWYHXUSPAIBRCJ"; turnover=' '}
let rotor2: rotor ={wiring="AJDKSIRUXBLHWTMCQGZNPYFVOE"; turnover=' '}
let rotor3: rotor ={wiring="BDFHJLCPRTXVZNYEIWGAKMUSQO"; turnover=' '}


let oriented_rotor1 : oriented_rotor= {rotor=rotor1; top_letter='A'}
let oriented_rotor2 : oriented_rotor= {rotor=rotor2; top_letter='A'}
let oriented_rotor3 : oriented_rotor= {rotor=rotor3; top_letter='A'}


 let config1: config= {refl="YRUHQSLDPXNGOKMIEBFZCWVJAT"; rotors=[oriented_rotor1;oriented_rotor2;oriented_rotor3]; plugboard=[] } 
 let config2: config= {refl="YRUHQSLDPXNGOKMIEBFZCWVJAT"; rotors=[]; plugboard=[] } 
 let config3: config= {refl="YRUHQSLDPXNGOKMIEBFZCWVJAT"; rotors=[oriented_rotor1;oriented_rotor2;oriented_rotor3]; plugboard=[('P', 'k')] } 

 let config4: config= {refl="ABCDEFGHIJKLMNOPQRSTUVWXYZ"; rotors=[]; plugboard=[('G', 'A')] } 

 let config5: config= {refl="ABCDEFGHIJKLMNOPQRSTUVWXYZ"; rotors=[]; plugboard=[] } 

let cipher_char_tests = [cipher_char_test "test1" config1 'G' 'P' ;
cipher_char_test "test2" config2 'G' 'L';
cipher_char_test "test3" config3 'G' 'k';
cipher_char_test "test4" config4 'G' 'G'; (*G->A and A->G*)
cipher_char_test "test5" config5 'K' 'K';
]


let step_tests = [ (* TODO: add your tests here *) ]
let cipher_tests = [ (* TODO: add your tests here *) ]

let tests =
  "test suite for A1"
  >::: List.flatten
         [
           index_tests;
           map_rl_tests;
           map_lr_tests;
           map_refl_tests;
           map_plug_tests;
           cipher_char_tests;
           step_tests;
           cipher_tests;
         ]

let _ = run_test_tt_main tests
