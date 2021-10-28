package com.validation

import cats.Show

sealed trait ExecutionStatus { status =>

  val subjectCode: Option[String] =
    status match {
      case ExecutionStatus.Success        => Option.empty
      case s: ExecutionStatus.Failed      => Option(s.subject)
    }

  val reasonCode: Option[String] =
    status match {
      case ExecutionStatus.Success        => Option.empty
      case s: ExecutionStatus.Failed      => Option(s.reason)
    }

  val subjectIdentifier: Option[String] =
    status match {
      case ExecutionStatus.Success        => Option.empty
      case s: ExecutionStatus.Failed      => s.identifier
    }

  val statusMessage: Option[String] =
    status match {
      case ExecutionStatus.Success        => Option.empty
      case s: ExecutionStatus.Failed      => s.message
    }


  val isSuccess: Boolean = this match {
    case ExecutionStatus.Success => true
    case _                       => false
  }

  val isFailed: Boolean = this match {
    case _: ExecutionStatus.Failed => true
    case _                                                      => false
  }
}

object ExecutionStatus {

  case object Success extends ExecutionStatus
  case class Failed(subject: String, reason: String, identifier: Option[String], message: Option[String])  extends ExecutionStatus

  def failed(subject: String, reason: String, identifier: String, message: String): ExecutionStatus =
    Failed(subject, reason, Option(identifier), Option(message))

  val SuccessName     = "EXECUTED_SUCCESS"
  val FailedName      = "FAILED"

  implicit val executionStatusShowInstance: Show[ExecutionStatus] =
    Show.show {
      case Success        => SuccessName
      case _: Failed      => FailedName
    }
}
