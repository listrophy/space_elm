require 'current_user'

class ApplicationController < ActionController::API
  include ActionController::Cookies
  include CurrentUser

  before_filter :set_current_user
  attr_reader :current_user

  def set_current_user
    @current_user = find_verified_user
  end
end
