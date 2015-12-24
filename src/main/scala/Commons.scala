import spray.httpx.SprayJsonSupport
import spray.json.AdditionalFormats
import spray.json.DefaultJsonProtocol
import spray.json.JsValue
import spray.json.JsonFormat
import spray.json.NullOptions
import spray.json.RootJsonFormat
import spray.json.pimpAny
import scala.collection.mutable.ListBuffer
import java.security.Key

object Constants {

  val serverPort = 9444
  val serverHost = "localhost"
  val serverURL = "http://" + serverHost + ":" + serverPort

  val ALGORITHM: String = "RSA"
  val charset: String = "ISO-8859-1"

  val totalUsers = 100
  val numOfFriends = (2 * (totalUsers / 100)).toInt + 5
  val initialAlbumsCount = 2
  val initialPostsCount = 3

  object Privacy {
    val Friends = "friends"
    val Private = "personal"
  }

  object PostTypes {
    val Default = "Default"
    val Photo = "Photo"
    val ProfileUpdate = "ProfileUpdate"
  }

  object messages {
    val success = "OK! Successfully completed"
    val created = "OK! user created successfully "
    val photoAdded = "OK! Successfully added the photo to user profile"
    val albumCreated = "OK! Album successfully created."
    val albumUpdated = "OK! Album updated successfully."
    val updated = "OK! details successfully updated"

    val noUser = "User does not exist!!"
    val noAlbum = "User does not have such an album!!"
    val noPermission = "You dont have permission to perform this operation"
    val userAlreadyPresent = "Username already exists"
    val albumCreationFailed = "Album creation failed"
    val photoAddFailed = "Photo addition to user album failed"
  }

  val places = Array[String]("Colorado", "Sweden", "Mumbai", "Romania", "Italy", "Paris")

  val prefix = "src\\main\\resources\\"
  val images = Array[String](prefix + "Hello.jpg", prefix + "Secure.jpg", prefix + "World.jpg")
}

object Relation extends Enumeration {
  type Relation = Value
  val Single, InARelation = Value
}

object Gender extends Enumeration {
  type Gender = Value
  val Male, Female = Value
}

object Notification {
  val PostType = "Post"
  val ProfileType = "Profile"
  val PhotoType = "Photo"
  val FriendAddType = "FriendAdd"
}

case class Notify(notifyType: String, key: String, value: String)

sealed trait Seal

case class UserLogin(username: String, password: String)

object UserLoginJsonSupport extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val PortofolioFormats = jsonFormat2(UserLogin)
}

//JSON Types
//found : Any required: spray.httpx.marshalling.ToResponseMarshallable

case class User(val userId: String, var firstName: String, var lastName: String,var age: Int,var gender: String,var relation: String)

case class FriendRequest(userId: String, frndId: String)

case class UsersList(userIds: Array[String])

case class UserPost(var uuid: String, postby: String,var  message: String, link: Option[String] = None, place: Option[String] = None, privacy: String, object_attachment: Option[String] = None, Type: String = Constants.PostTypes.Default)

case class PostAdded(uuid: String, message: String)

case class UserPage(posts: Array[UserPost])

case class Album(userId: String, albumId: String, coverPhoto: Option[String] = None, createdTime: Option[String] = None, description: Option[String] = None, place: Option[String] = None, updateTime: Option[String] = None, var photos: Option[Array[String]] = None)

case class Photo(userId: String, albumId: String, photoId: String, src: String, message: Option[String] = None, place: Option[String] = None, noStory: Boolean = false)

case class Success(reason: String)

case class Error(reason: String)

case class AuthReq(userId: String, key: String)

case class AuthToken(userId: String, token: String)

object jsonProtocol extends DefaultJsonProtocol with SprayJsonSupport with NullOptions with AdditionalFormats {
  implicit val FriendReqFormat = jsonFormat2(FriendRequest)

  implicit object NewUsersResFormat extends RootJsonFormat[UsersList] {
    def read(value: JsValue) = UsersList(value.convertTo[Array[String]])

    def write(f: UsersList) = f.userIds.toJson
  }

  implicit val UserFormat = jsonFormat6(User)
  implicit val postFormat = jsonFormat8(UserPost)
  implicit val postAddedFormat = jsonFormat2(PostAdded)

  implicit object postList extends RootJsonFormat[UserPage] {
    def read(value: JsValue) = UserPage(value.convertTo[Array[UserPost]])

    def write(f: UserPage) = f.posts.toJson
  }

  implicit def userPageFormat[Post: JsonFormat] = jsonFormat1(UserPage.apply)

  implicit val albumFormat = jsonFormat8(Album)
  implicit val photoFormat = jsonFormat7(Photo)
  implicit val errorFormat = jsonFormat1(Error)
  implicit val successFormat = jsonFormat1(Success)
  implicit val AuthReqFormat = jsonFormat2(AuthReq)
  implicit val AuthTokenFormat = jsonFormat2(AuthToken)

}

case class findProfile(userId: String)

//extends Seal
case class getUserPage(userId: String)

case class getFriendsList(userId: String, frndId: String)

case class deleteAlbum(userId: String, albumId: String)

case class getPhotos(photoId: String)

case class getUserAlbums(userId: String, frndId: String)

case class systemSetup()

class PictureAlbum(val ownerId: String, val albumId: String) {

  var coverPhoto: Option[String] = None
  var createdTime: String = null
  var description: Option[String] = None
  var place: Option[String] = None
  var updateTime: String = null
  var photos: ListBuffer[String] = new ListBuffer[String]

  def populate(pcoverPhoto: Option[String] = None, pdescription: Option[String] = None, pplace: Option[String] = None) {
    coverPhoto = pcoverPhoto
    description = pdescription
    place = pplace
    createdTime = System.currentTimeMillis().toString()
    updateTime = System.currentTimeMillis().toString()
  }

  def update(pcoverPhoto: Option[String] = None, pdescription: Option[String] = None, pplace: Option[String] = None) {
    coverPhoto = pcoverPhoto
    description = pdescription
    place = pplace
    updateTime = System.currentTimeMillis().toString()
  }

  def addPicture(pic: Picture) {
    if (pic != null) {
      photos.append(pic.photoId)
    }
  }

}

class Picture(val albumId: String, val photoId: String, val src: String) {

  var message: Option[String] = None
  var place: Option[String] = None
  var nostory: Boolean = false

  def populate(pmessage: Option[String] = None, pplace: Option[String] = None, pnostory: Boolean = false) {
    nostory = pnostory
    message = pmessage
    place = pplace
  }
}

