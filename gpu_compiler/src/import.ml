include Composition_infix

include struct
  open Zaia_spirv_raw
  module Spirv_generated = Spirv_generated
  module Version = Spirv_generated.Requirements.Version
  module Extension = Spirv_generated.Requirements.Extension
  module Capability = Spirv_generated.Operand_kind.Payload.Capability

  include struct
    module Spirv_instruction = Spirv_generated.Instruction
  end
end
