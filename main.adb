pragma SPARK_Mode (On);

with MyCommandLine;
with MyString;
with MyStringTokeniser;
with StringToInteger;
with PIN;
with MemoryStore;

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO;

with CalculatorCommands; use CalculatorCommands;

procedure Main is
   --  Helper instantiation for bounded lines
   --package Lines is new MyString (Max_MyString_Length => 2048);
   S : Lines.MyString;

   -- 分词数组
   T : MyStringTokeniser.TokenArray (1 .. 5);

   NumTokens : Natural;

   -- 存储与 PIN 状态
   Mem       : MemoryStore.Database;
   MasterPIN : PIN.PIN;
   Unlocked  : Boolean := False;

   -- 操作数栈定义
   Stack : CalculatorCommands.Operand_Stack := (others => 0);

   Stack_Top : Natural := 0;

begin
   -- 启动时检查是否提供主密码
   if MyCommandLine.Argument_Count < 1 then
      Put_Line ("Error: No master PIN provided.");
      return;
   end if;

   -- 设置主密码
   declare
      Arg : constant String := MyCommandLine.Argument (1);
   begin
      if Arg'Length = 4 and then (for all C of Arg => C in '0' .. '9') then
         MasterPIN := PIN.From_String (Arg);
      else
         Put_Line ("Invalid master PIN: must be a 4-digit numeric string.");
         return;
      end if;
   end;

   -- 初始化内存、主循环
   MemoryStore.Init (Mem);
   loop
      if Unlocked then
         Put ("unlocked> ");
      else
         Put ("locked> ");
      end if;

      Lines.Get_Line (S);
      if Lines.Length (S) > 2048 then
         Put_Line ("Error: Input too long.");
         return;
      end if;
      T := (others => (Start => 1, Length => 0));
      MyStringTokeniser.Tokenise
        (Trim_Trailing_Spaces (Lines.To_String (S)), T, NumTokens);
      if NumTokens /= 0 then
         declare
            Cmd : constant String := Get_Token_String (S, T (1));
         begin
            if Cmd'Length > 20 then
               Put_Line ("Command too long.");
            elsif Cmd = "unlock" then
               if not Unlocked then
                  Handle_Unlock (S, T, MasterPIN, Unlocked, Numtokens);
               else
                  Put_Line ("Already unlocked.");
               end if;

            elsif Cmd = "lock" then
               if Unlocked then
                  Handle_lock (S, T, MasterPIN, Unlocked, Numtokens);
               else
                  Put_Line ("Already locked.");
               end if;

            elsif Cmd = "push1" then
               if not Unlocked then
                  Put_Line ("Command not allowed: calculator is locked.");
               else
                  Handle_Push1 (S, T, Stack, Stack_Top, Numtokens);
               end if;

            elsif Cmd = "push2" then
               if not Unlocked then
                  Put_Line ("Command not allowed: calculator is locked.");
               else
                  Handle_Push2 (S, T, Stack, Stack_Top, NumTokens);
               end if;
            --  if Stack_Top = 0 then
            --     Ada.Text_IO.Put_Line ("Stack is empty.");
            --  else
            --     Ada.Text_IO.Put_Line ("Current stack (bottom to top):");
            --     for I in 1 .. Stack_Top loop
            --        Ada.Text_IO.Put (Integer'Image (Stack (I)));
            --        Ada.Text_IO.Put (" ");
            --     end loop;
            --     Ada.Text_IO.New_Line;
            --  end if;

            elsif Cmd = "pop" then
               if not Unlocked then
                  Put_Line ("Command not allowed: calculator is locked.");
               else
                  Handle_Pop (Stack, Stack_Top, NumTokens);
               end if;

            elsif Cmd = "+" then
               if not Unlocked then
                  Put_Line ("Command not allowed: calculator is locked.");
               else
                  Handle_Add (Stack, Stack_Top, Unlocked);
               end if;

            elsif Cmd = "-" then
               if not Unlocked then
                  Put_Line ("Command not allowed: calculator is locked.");
               else
                  Handle_Subtract (Stack, Stack_Top, Unlocked);
               end if;

            elsif Cmd = "*" then
               if not Unlocked then
                  Put_Line ("Command not allowed: calculator is locked.");
               else
                  Handle_Multiply (Stack, Stack_Top, Unlocked);
               end if;

            elsif Cmd = "/" then
               if not Unlocked then
                  Put_Line ("Command not allowed: calculator is locked.");
               else
                  Handle_Divide (Stack, Stack_Top, Unlocked);
               end if;

            elsif Cmd = "storeTo" then
               if not Unlocked then
                  Put_Line ("Command not allowed: calculator is locked.");
               else
                  Handle_StoreTo
                    (S, T, Stack, Stack_Top, Unlocked, NumTokens, Mem);
               end if;

            elsif Cmd = "list" then
               if not Unlocked then
                  Put_Line ("Command not allowed: calculator is locked.");
               else
                  Handle_List (Unlocked, NumTokens, Mem);
               end if;

            elsif Cmd = "remove" then
               if not Unlocked then
                  Put_Line ("Command not allowed: calculator is locked.");
               else
                  Handle_Remove (S, T, Unlocked, NumTokens, Mem);
               end if;

            elsif Cmd = "loadFrom" then
               if not Unlocked then
                  Put_Line ("Command not allowed: calculator is locked.");
               else
                  Handle_LoadFrom
                    (S, T, Stack, Stack_Top, Unlocked, NumTokens, Mem);
               end if;
            else
               Put_Line ("Unknown command: " & Cmd);
            end if;

         end;
      else
         Put_Line ("No command entered.");
      end if;
   end loop;
end Main;
