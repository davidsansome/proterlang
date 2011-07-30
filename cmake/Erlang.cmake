find_program(ERLANG_COMPILE erlc)

macro(add_erlang sources_var)
  foreach(arg ${ARGN})
    get_filename_component(basename ${arg} NAME_WE)
    get_filename_component(abspath ${arg} ABSOLUTE)
    set(beam ${CMAKE_CURRENT_BINARY_DIR}/${basename}.beam)

    add_custom_command(
      OUTPUT ${beam}
      COMMAND ${ERLANG_COMPILE}
        -I ${CMAKE_BINARY_DIR}/src
        -I ${CMAKE_SOURCE_DIR}/src
        -o ${CMAKE_CURRENT_BINARY_DIR}/
        ${abspath}
      DEPENDS ${arg}
    )

    list(APPEND ${sources_var} ${beam})
  endforeach(arg)
endmacro(add_erlang)

macro(compile_erlang target)
  add_library(dummy_${target} STATIC ${ARGN})
  set_target_properties(dummy_${target} PROPERTIES LINKER_LANGUAGE C)
endmacro(compile_erlang)

add_custom_target(test)

macro(test_erlang module)
  add_custom_target(test_${module}
    COMMAND erl -pa ${CMAKE_CURRENT_BINARY_DIR}
                -pa ${CMAKE_BINARY_DIR}/src
                -run ${module} test
                -run init stop
                -noshell
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${module}.beam
  )
  add_dependencies(test test_${module})
endmacro(test_erlang)
