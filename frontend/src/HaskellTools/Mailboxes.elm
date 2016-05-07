module HaskellTools.Mailboxes (..) where

import HaskellTools.Action exposing (..)

actionsMailbox : Signal.Mailbox Action
actionsMailbox =
  Signal.mailbox NoOp
