package com

import zio.IO
import com.validation.Validator.Error

package object validation {
  type Validator[I, C, O] = I => IO[Error[C], O]
  type Check[I]           = I => Option[ExecutionStatus]
}
