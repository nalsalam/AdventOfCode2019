# Day 7
Here’s some help:
  
  Essentially this works as such:
  Create 5 instances of your intcode computer (with their own separate Program Counters and memory), A - E. During the execution, they will at various points try to OUTPUT or INPUT (opcodes 3 + 4).
They then run as follows:
  •	A takes 2 inputs (phase, "0"), outputs a value, then tries to take a 3rd that you don't have yet, so pause it (store the memory and PC AS-IS) and move on to B.
•	B takes 2 inputs (phase, Output from A), outputs a value, then tries to take a 3rd that you don't have yet, so pause it and move on to C
•	C, repeats the same, but using "Output from B" for the second input.
•	D, repeats the same, but using "Output from C" for the second input.
•	E, repeats the same, but using "Output from D" for the second input.
•	Now you can loop back to A. You now have the value from E that you can use to provide that 3rd input, it'll run for a while (continuing from where you paused it earlier, not from the beginning) 
and eventually it will output another value and wait for a 4th input. Pause it, move on to B.
•	B's 3rd input is output A gave after being given it's 3rd input from E's output, it will do the same again, consume the input, generate an output, then wait for a 4th input.
And so on, each amplifier will consume the previous output and generate a new one then wait for more input.
Eventually, they'll all stop asking for input and actually halt (Opcode 99) all basically at the same time in order (A, then B, then C...) once E has halted take the output and that's your answer.
(When they halt, they'll provide an output before halting, so you'll still have something to pass on to the next amplifier when it asks for an input)

(Source: https://www.reddit.com/r/adventofcode/comments/e7paah/2019_day_7_part_2_confused_on_wording_need_help/)

To answer the first part of the problem, I sent each computer a list with two items, and had my input pop() off that list.
So each “Amp” “ran” twice:
  
Ok, that makes sense. I have a plan for creating the feedback loop, but not exiting it. If you exit the feedback loop when E hits opcode 99, then what is E’s output? 
  
  
If you’re keeping track of the outputs, then it’s whatever was last output by Amp E before Amp A, Amp B, Amp C, Amp D, and Amp E sequentially halt.
That is, I think the last output from Amp E that gets feed into Amp A and starts the halting cascade is what you send to the Thruster.
(But I could certainly be wrong about that!)

