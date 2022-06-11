{-# language RecordWildCards #-}
{-# language NamedFieldPuns #-}
module PID where

data Params = Params
    { p_kp, p_ki, p_kd :: Double }

data PID = PID
    { pid_params :: Params
        -- ^ PID controller tuning parameters
    , pid_setpoint :: Double
        -- ^ target value
    , pid_lastError :: Double
        -- ^ error at last update
    , pid_sumError :: Double
        -- ^ sum of errors
    , pid_output :: Double
        -- ^ current output
    }


new :: Params -> Double -> PID
new pid_params pid_setpoint = PID
    { pid_lastError = 0
    , pid_sumError = 0
    , pid_output = 0
    , ..
    }


output
    :: PID
    -> Double
output PID{..} = pid_output


-- | simplest and worst PID update algorithm
update
    :: Double -- ^ input
    -> Double -- ^ time step
    -> PID
    -> PID
update input dt pid@PID{pid_params=Params{..}, ..} =
    let
        new_error = pid_setpoint - input
        dError = (new_error - pid_lastError) / dt
        new_sumError = pid_sumError + dt * new_error
        new_output = p_kp * new_error + p_ki * new_sumError + p_kd * dError
    in pid
        { pid_lastError = new_error
        , pid_sumError = new_sumError
        , pid_output = new_output
        }
