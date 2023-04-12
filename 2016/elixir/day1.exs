defmodule Day1 do
  @directions [{:north, 0, 1}, {:east, 1, 0}, {:south, 0, -1}, {:west, -1, 0}]

  def spin(direction, turn) do
    offset = if String.starts_with?(turn, "R"), do: 1, else: -1
    index = Enum.find_index(@directions, &(&1 == direction))
    new_index = rem(index + offset, Enum.count(@directions))
    Enum.at(@directions, new_index)
  end

  def scenario_one(file_name) do
    position =
      file_name
      |> File.read!()
      |> String.trim()
      |> String.split(", ")
      |> Enum.reduce({Enum.at(@directions, 0), 0, 0}, fn command, {direction, x, y} ->
        direction = spin(direction, command)
        distance = String.to_integer(String.slice(command, 1..-1))
        {direction, x + elem(direction, 1) * distance, y + elem(direction, 2) * distance}
      end)

    abs(elem(position, 1)) + abs(elem(position, 2))
  end

  def scenario_two(file_name) do
    position =
      file_name
      |> File.read!()
      |> String.trim()
      |> String.split(", ")
      |> Enum.reduce_while(
        {Enum.at(@directions, 0), 0, 0, []},
        fn command, {direction, x, y, history} ->
          direction = spin(direction, command)
          distance = String.to_integer(String.slice(command, 1..-1))

          step = fn remaining, new_history, new_position, recur ->
            if remaining == 0 or Enum.member?(new_history, new_position) do
              {new_history, new_position}
            else
              recur.(
                remaining - 1,
                [new_position | new_history],
                {elem(new_position, 0) + elem(direction, 1),
                 elem(new_position, 1) + elem(direction, 2)},
                recur
              )
            end
          end

          {new_history, new_position} = step.(distance, history, {x, y}, step)
          continue = if Enum.member?(history, new_position), do: :halt, else: :cont
          new_state = {direction, elem(new_position, 0), elem(new_position, 1), new_history}
          {continue, new_state}
        end
      )

    abs(elem(position, 1)) + abs(elem(position, 2))
  end
end

IO.puts("Scenario One: #{Day1.scenario_one("../input/day1.txt")}")
IO.puts("Scenario Two: #{Day1.scenario_two("../input/day1.txt")}")
