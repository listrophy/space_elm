module CurrentUser
  def find_verified_user
    id = cookies.signed[:user_id]

    user =
      if id
        User.find_by(id: id)
      end

    if user.present?
      user
    else
      User.create.tap do |u|
        cookies.signed[:user_id] = u.id
      end
    end
  end
end
